/**
  * Licensed to Gravity.com under one
  * or more contributor license agreements.  See the NOTICE file
  * distributed with this work for additional information
  * regarding copyright ownership.  Gravity.com licenses this file
  * to you under the Apache License, Version 2.0 (the
  * "License"); you may not use this file except in compliance
  * with the License.  You may obtain a copy of the License at
  *
  * http://www.apache.org/licenses/LICENSE-2.0
  *
  * Unless required by applicable law or agreed to in writing, software
  * distributed under the License is distributed on an "AS IS" BASIS,
  * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
  * See the License for the specific language governing permissions and
  * limitations under the License.
  */
package com.gravity.goose.images

import java.io.{File, IOException}
import java.net.{MalformedURLException, URL}
import java.util
import java.util.regex.{Matcher, Pattern}

import com.gravity.goose.network.HtmlFetcher
import com.gravity.goose.text.string
import com.gravity.goose.{Article, Configuration}
import com.typesafe.scalalogging.StrictLogging
import org.apache.http.client.HttpClient
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.protocol.HttpClientContext
import org.apache.http.protocol.{BasicHttpContext, HttpContext}
import org.apache.http.{Header, HttpEntity, HttpResponse}
import org.jsoup.nodes.{Document, Element}
import org.jsoup.select.Elements

import scala.collection.JavaConverters._
import scala.collection.mutable

/**
  * Created by Jim Plush
  * User: jim
  * Date: 8/18/11
  */

case class DepthTraversal(node: Element, parentDepth: Int, siblingDepth: Int)

/**
  * This image extractor will attempt to find the best image nearest the article.
  * Unfortunately this is a slow process since we're actually downloading the image itself
  * to inspect it's actual height/width and area metrics since most of the time these aren't
  * in the image tags themselves or can be falsified.
  * We'll weight the images in descending order depending on how high up they are compared to the top node content
  *
  * //todo this is a straight java to scala conversion, need to add the nicities of scala, all these null checks make me dizzy
  */
class StandardImageExtractor(httpClient: HttpClient, article: Article, config: Configuration) extends ImageExtractor with StrictLogging {


  /**
    * holds the document that we're extracting the image from
    */
  private var doc: Document = null

  /**
    * What's the minimum bytes for an image we'd accept is
    */
  private var minBytesForImages: Int = 0
  /**
    * location to store temporary image files if need be
    */
  private var tempStoragePath: String = null

  /**
    * this lists all the known bad button names that we have
    */
  var matchBadImageNames: Matcher = null
  val NODE_ID_FORMAT: String = "tag: %s class: %s ID: %s"
  val KNOWN_IMG_DOM_NAMES = "yn-story-related-media" :: "cnn_strylccimg300cntr" :: "big_photo" :: "ap-smallphoto-a" :: Nil

  var sb: StringBuilder = new StringBuilder
  // create negative elements
  sb.append(".html|.gif|.ico|button|twitter.jpg|facebook.jpg|ap_buy_photo|digg.jpg|digg.png|delicious.png|facebook.png|reddit.jpg|doubleclick|diggthis|diggThis|adserver|/ads/|ec.atdmt.com")
  sb.append("|mediaplex.com|adsatt|view.atdmt")
  matchBadImageNames = Pattern.compile(sb.toString()).matcher(string.empty)

  /**
    * holds the result of our image extraction
    */
  val image = new Image
  /**
    * the webpage url that we're extracting content from
    */
  val targetUrl = article.finalUrl
  /**
    * stores a hash of our url for reference and image processing
    */
  val linkhash = article.linkhash


  override def getBestImage(doc: Document, topNode: Element): Image = {
    this.doc = doc
    logger.debug("Starting to Look for the Most Relavent Image")
    if (image.getImageSrc == "") {
      this.checkForKnownElements()
    }
    if (image.getImageSrc == "") {
      this.checkForLargeImages(topNode, 0, 0)
    }
    if (image.getImageSrc == "") {
      this.checkForMetaTag
    }
    image
  }

  private def checkForMetaTag: Boolean = {
    if (this.checkForLinkTag) {
      return true
    }
    if (this.checkForOpenGraphTag) {
      return true
    }
    logger.debug("unable to find meta image")
    false
  }

  /**
    * checks to see if we were able to find open graph tags on this page
    *
    * @return
    */
  private def checkForOpenGraphTag: Boolean = {
    try {
      val meta: Elements = doc.select("meta[property~=og:image]")
      for (item <- meta.asScala) {
        if (item.attr("content").length < 1) {
          return false
        }
        val imagePath: String = this.buildImagePath(item.attr("content"))
        this.image.imageSrc = imagePath
        this.image.imageExtractionType = "opengraph"
        this.image.confidenceScore = 100
        this.image.bytes = this.getBytesForImage(imagePath)
        logger.trace("open graph tag found, using it")

        return true
      }
      false
    }
    catch {
      case e: Exception =>
        e.printStackTrace()
        return false
    }
  }

  /**
    * checks to see if we were able to find open graph tags on this page
    *
    * @return
    */
  private def checkForLinkTag: Boolean = {
    try {
      val meta: Elements = doc.select("link[rel~=image_src]")
      for (item <- meta.asScala) {
        if (item.attr("href").length < 1) {
          return false
        }
        this.image.imageSrc = this.buildImagePath(item.attr("href"))
        this.image.imageExtractionType = "linktag"
        this.image.confidenceScore = 100
        this.image.bytes = getBytesForImage(buildImagePath(item.attr("href")))
        logger.trace("link tag found, using it")

        return true
      }
      false
    }
    catch {
      case e: Exception =>
        logger.error(e.toString, e)
        false
    }
  }

  def getAllImages: util.ArrayList[Element] = {
    null
  }

  def getImagesFromNode(node: Element): Option[Elements] = {
    val images: Elements = node.select("img")

    if (images == null || images.size < 1) {
      None
    } else {
      Some(images)
    }
  }


  def getImageCandidates(node: Element): Option[util.ArrayList[Element]] = {

    for {
      n <- getNode(node)
      images <- getImagesFromNode(node)
      filteredImages <- filterBadNames(images)
      goodImages <- findImagesThatPassByteSizeTest(filteredImages)
    } {
      return Some(goodImages)
    }
    None

  }

  def getDepthLevel(node: Element, parentDepth: Int, siblingDepth: Int): Option[DepthTraversal] = {
    val MAX_PARENT_DEPTH = 2
    if (parentDepth > MAX_PARENT_DEPTH) {
      logger.trace("ParentDepth is greater than %d, aborting depth traversal".format(MAX_PARENT_DEPTH))
      None
    } else {
      try {
        val siblingNode = node.previousElementSibling()
        if (siblingNode == null) throw new NullPointerException
        Some(DepthTraversal(siblingNode, parentDepth, siblingDepth + 1))
      } catch {
        case e: NullPointerException =>
          if (node != null) {
            Some(DepthTraversal(node.parent, parentDepth + 1, 0))
          } else {
            None
          }
      }
    }
  }

  /**
    * although slow the best way to determine the best image is to download them and check the actual dimensions of the image when on disk
    * so we'll go through a phased approach...
    * 1. get a list of ALL images from the parent node
    * 2. filter out any bad image names that we know of (gifs, ads, etc..)
    * 3. do a head request on each file to make sure it meets our bare requirements
    * 4. any images left over let's do a full GET request, download em to disk and check their dimensions
    * 5. Score images based on different factors like height/width and possibly things like color density
    *
    * @param node
    */
  private def checkForLargeImages(node: Element, parentDepthLevel: Int, siblingDepthLevel: Int): Unit = {
    logger.trace("Checking for large images - parent depth %d sibling depth: %d".format(parentDepthLevel, siblingDepthLevel))


    getImageCandidates(node) match {
      case Some(goodImages) =>
        logger.trace("checkForLargeImages: After findImagesThatPassByteSizeTest we have: " + goodImages.size + " at parent depth: " + parentDepthLevel)
        val scoredImages = downloadImagesAndGetResults(goodImages, parentDepthLevel)
        var highScoreImage: Element = null
        scoredImages.foreach {
          case (key, value) =>
            if (highScoreImage == null) {
              highScoreImage = key
            } else {
              if (value > scoredImages(highScoreImage)) {
                highScoreImage = key
              }
            }
        }

        if (highScoreImage != null) {
          val f: File = new File(highScoreImage.attr("tempImagePath"))
          this.image.topImageNode = highScoreImage
          this.image.imageSrc = buildImagePath(highScoreImage.attr("src"))
          this.image.imageExtractionType = "bigimage"
          this.image.bytes = f.length.asInstanceOf[Int]
          if (scoredImages.nonEmpty) {
            this.image.confidenceScore = 100 / scoredImages.size
          }
          else {
            this.image.confidenceScore = 0
          }
          logger.trace("High Score Image is: " + buildImagePath(highScoreImage.attr("src")))
        } else {
          getDepthLevel(node, parentDepthLevel, siblingDepthLevel) match {
            case Some(depthObj) =>
              checkForLargeImages(depthObj.node, depthObj.parentDepth, depthObj.siblingDepth)
            case None => logger.trace("Image iteration is over!")
          }
        }
      case None =>

        getDepthLevel(node, parentDepthLevel, siblingDepthLevel) match {
          case Some(depthObj) =>
            checkForLargeImages(depthObj.node, depthObj.parentDepth, depthObj.siblingDepth)
          case None => logger.trace("Image iteration is over!")
        }
    }
  }

  def getNode(node: Element): Option[Element] = Option(node)

  /**
    * loop through all the images and find the ones that have the best bytez to even make them a candidate
    *
    * @param images
    * @return
    */
  private def findImagesThatPassByteSizeTest(images: util.ArrayList[Element]): Option[util.ArrayList[Element]] = {
    var cnt: Int = 0
    val goodImages: util.ArrayList[Element] = new util.ArrayList[Element]

    images.asScala.foreach(image => {
      try {
        if (cnt > 30) {
          logger.trace("Abort! they have over 30 images near the top node: " + this.doc.baseUri)
          return Some(goodImages)
        }
        val bytes: Int = getBytesForImage(image.attr("src"))

        val MAX_BYTES_SIZE: Int = 15728640
        if ((bytes == 0 || bytes > minBytesForImages) && bytes < MAX_BYTES_SIZE) {
          logger.trace("findImagesThatPassByteSizeTest: Found potential image - size: " + bytes + " src: " + image.attr("src"))
          goodImages.add(image)
        }
        else {
          logger.trace(" Removing image: " + image.attr("src"))
          image.remove()
        }
      } catch {
        case e: Exception => logger.warn(e.toString, e)
      }
      cnt += 1
    })

    logger.trace(" Now leaving findImagesThatPassByteSizeTest")
    if (goodImages != null && goodImages.size > 0) Some(goodImages) else None
  }

  /**
    * takes a list of image elements and filters out the ones with bad names
    *
    * @param images
    * @return
    */
  private def filterBadNames(images: Elements): Option[util.ArrayList[Element]] = {
    val goodImages: util.ArrayList[Element] = new util.ArrayList[Element]
    for (image <- images.asScala) {
      if (this.isOkImageFileName(image)) {
        goodImages.add(image)
      }
      else {
        image.remove()
      }
    }
    if (goodImages != null && goodImages.size > 0) Some(goodImages) else None
  }

  /**
    * will check the image src against a list of bad image files we know of like buttons, etc...
    *
    * @return
    */
  private def isOkImageFileName(imageNode: Element): Boolean = {
    var imgSrc: String = imageNode.attr("src")
    if (string.isNullOrEmpty(imgSrc)) {
      return false
    }
    matchBadImageNames.reset(imgSrc)
    if (matchBadImageNames.find) {
      logger.debug("Found bad filename for image: " + imgSrc)
      false
    } else {
      true
    }
  }

  /**
    * in here we check for known image contains from sites we've checked out like yahoo, techcrunch, etc... that have
    * known  places to look for good images.
    * //todo enable this to use a series of settings files so people can define what the image ids/classes are on specific sites
    */
  def checkForKnownElements(): Unit = {

    var knownImage: Element = null
    logger.trace("Checking for known images from large sites")

    for (knownName <- KNOWN_IMG_DOM_NAMES) {

      try {
        var known: Element = article.rawDoc.getElementById(knownName)
        if (known == null) {
          known = article.rawDoc.getElementsByClass(knownName).first
        }
        if (known != null) {
          val mainImage: Element = known.getElementsByTag("img").first
          if (mainImage != null) {
            knownImage = mainImage
            logger.debug("Got Image: " + mainImage.attr("src"))
          }
        }

      }
      catch {
        case e: NullPointerException =>
          logger.debug(e.toString, e)
      }
    }
    if (knownImage != null) {
      val knownImgSrc: String = knownImage.attr("src")
      this.image.imageSrc = this.buildImagePath(knownImgSrc)
      this.image.imageExtractionType = "known"
      this.image.confidenceScore = 90
      this.image.bytes = this.getBytesForImage(knownImgSrc)
    }
    else {
      logger.debug("No known images found")
    }

  }

  /**
    * This method will take an image path and build out the absolute path to that image
    * using the initial url we crawled so we can find a link to the image if they use relative urls like ../myimage.jpg
    *
    * @param image
    * @return
    */
  private def buildImagePath(image: String): String = {
    var pageURL: URL = null
    var newImage: String = image.replace(" ", "%20")
    try {
      pageURL = new URL(this.targetUrl)
      var imageURL: URL = new URL(pageURL, image)
      newImage = imageURL.toString
    }
    catch {
      case e: MalformedURLException =>
        logger.error("Unable to get Image Path: " + image)
    }
    newImage
  }

  /**
    * does the HTTP HEAD request to get the image bytes for this images
    *
    * @param src
    * @return
    */
  private def getBytesForImage(src: String): Int = {
    var bytes: Int = 0
    var httpget: HttpGet = null
    try {
      var link: String = this.buildImagePath(src)
      link = link.replace(" ", "%20")
      val localContext: HttpContext = new BasicHttpContext
      localContext.setAttribute(HttpClientContext.COOKIE_STORE, HtmlFetcher.emptyCookieStore)
      httpget = new HttpGet(link)
      var response: HttpResponse = null
      response = httpClient.execute(httpget, localContext)
      val entity: HttpEntity = response.getEntity
      bytes = this.minBytesForImages + 1
      try {
        val currentBytes: Int = entity.getContentLength.asInstanceOf[Int]
        val contentType: Header = entity.getContentType
        if (contentType.getValue.contains("image")) {
          bytes = currentBytes
        }
      }
      catch {
        case e: NullPointerException =>
          logger.warn("SRC: " + src + " " + e.toString, e)
      }
    }
    catch {
      case e: Exception =>
        logger.warn("BIG SRC: " + src + " " + e.toString, e)
    }
    finally {
      try {
        httpget.abort()
      }
      catch {
        case e: NullPointerException =>
          logger.error("HttpGet is null, can't abortz")
      }
    }
    bytes
  }

  /**
    * download the images to temp disk and set their dimensions
    * <p/>
    * we're going to score the images in the order in which they appear so images higher up will have more importance,
    * we'll count the area of the 1st image as a score of 1 and then calculate how much larger or small each image after it is
    * we'll also make sure to try and weed out banner type ad blocks that have big widths and small heights or vice versa
    * so if the image is 3rd found in the dom it's sequence score would be 1 / 3 = .33 * diff in area from the first image
    *
    * @param images
    * @return
    */

  private def downloadImagesAndGetResults(images: util.ArrayList[Element], depthLevel: Int): mutable.HashMap[Element, Float] = {
    val imageResults: mutable.HashMap[Element, Float] = new mutable.HashMap[Element, Float]
    var cnt: Int = 1
    var initialArea: Float = 0

    for (image <- images.asScala) {
      var continueVar = true // major haxor during java to scala conversion -> this whole section needs a rewrite
      if (cnt > 30) {
        logger.debug("over 30 images attempted, that's enough for now")
        return imageResults
      }
      try {
        val imageSource: String = this.buildImagePath(image.attr("src"))
        val localSrcPath: String = ImageSaver.storeTempImage(this.httpClient, this.linkhash, imageSource, config)
        if (localSrcPath == null) {
          logger.debug("unable to store this image locally: IMGSRC: " + image.attr("src") + " BUILD SRC: " + imageSource)
          continueVar = false
        }
        logger.debug("Starting image: " + localSrcPath)
        var width: Int = 0
        var height: Int = 0
        if (continueVar) {
          image.attr("tempImagePath", localSrcPath)
          try {
            var imageDims: ImageDetails = ImageUtils.getImageDimensions(config.imagemagickIdentifyPath, localSrcPath)
            width = imageDims.getWidth
            height = imageDims.getHeight
            if (depthLevel > 1) {
              if (width < 300) {
                logger.debug("going depthlevel: " + depthLevel + " and img was only: " + width + " wide: " + localSrcPath)
                continueVar = false
              }
            }
          }
          catch {
            case e: IOException =>
              throw e
          }
        }
        if (continueVar) {
          if (this.isBannerDimensions(width, height)) {
            logger.debug(image.attr("src") + " seems like a fishy image dimension wise, skipping it")
            image.remove()
            continueVar = false
          }
        }
        if (continueVar) {
          if (width < 50) {
            logger.debug(image.attr("src") + " is too small width: " + width + " removing..")
            image.remove()
            continueVar = false
          }
        }
        if (continueVar) {
          val sequenceScore: Float = (1).asInstanceOf[Float] / cnt
          val area: Int = width * height
          var totalScore: Float = 0
          if (initialArea == 0) {
            // give the initial image a little area boost as well
            initialArea = area * 1.48.asInstanceOf[Float]
            totalScore = 1
          }
          else {
            val areaDifference: Float = area.asInstanceOf[Float] / initialArea
            totalScore = sequenceScore.asInstanceOf[Float] * areaDifference
          }
          logger.trace(imageSource + " Area is: " + area + " sequence score: " + sequenceScore + " totalScore: " + totalScore)
          cnt += 1

          imageResults.put(image, totalScore)
        }
      }
      catch {
        case e: SecretGifException =>
        case e: Exception => logger.warn(e.toString, e)
      }
    }
    imageResults
  }


  /**
    * returns true if we think this is kind of a bannery dimension
    * like 600 / 100 = 6 may be a fishy dimension for a good image
    *
    * @param width
    * @param height
    */
  private def isBannerDimensions(width: Int, height: Int): Boolean = {
    if (width == height) {
      return false
    }
    if (width > height) {
      val diff: Float = (width.asInstanceOf[Float] / height.asInstanceOf[Float])
      if (diff > 5) {
        return true
      }
    }
    if (height > width) {
      val diff: Float = height.asInstanceOf[Float] / width.asInstanceOf[Float]
      if (diff > 5) {
        return true
      }
    }
    false
  }

  def getMinBytesForImages: Int = {
    minBytesForImages
  }

  def setMinBytesForImages(minBytesForImages: Int): Unit = {
    this.minBytesForImages = minBytesForImages
  }

  def getTempStoragePath: String = {
    tempStoragePath
  }

  def setTempStoragePath(tempStoragePath: String): Unit = {
    this.tempStoragePath = tempStoragePath
  }


}