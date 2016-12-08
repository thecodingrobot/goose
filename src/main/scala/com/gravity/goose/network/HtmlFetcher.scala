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

package com.gravity.goose.network

import java.io._
import java.net.{SocketException, SocketTimeoutException, URLConnection}
import java.util.Date

import com.gravity.goose.Configuration
import com.typesafe.scalalogging.StrictLogging
import org.apache.commons.io.IOUtils
import org.apache.http.client.config.{CookieSpecs, RequestConfig}
import org.apache.http.client.methods.HttpGet
import org.apache.http.client.protocol.HttpClientContext
import org.apache.http.client.{CookieStore, HttpClient}
import org.apache.http.config.SocketConfig
import org.apache.http.cookie.Cookie
import org.apache.http.entity.ContentType
import org.apache.http.impl.client.{DefaultHttpRequestRetryHandler, HttpClients}
import org.apache.http.impl.conn.PoolingHttpClientConnectionManager
import org.apache.http.message.BasicHeader
import org.apache.http.protocol.{BasicHttpContext, HttpContext}
import org.apache.http.util.EntityUtils
import org.apache.http.{HttpEntity, HttpHeaders, HttpResponse}

import scala.collection.JavaConverters._

/**
  * User: Jim Plush
  * Date: 12/16/10
  * This guy is kind of a doozy because goose is meant to pull millions of articles per day so the legitimacy of these links
  * is in question. For example many times you'll see mp3, mov, wav, etc.. files mislabeled as HTML with HTML content types,
  * only through inspection of the actual content will you learn what the real type of content is. Also spam sites could
  * contain up to 1GB of text that is just wasted resources so we set a max bytes level on how much content we're going
  * to try and pull back before we say screw it.
  */
object HtmlFetcher extends AbstractHtmlFetcher with StrictLogging {
  /**
    * holds a reference to our override cookie store, we don't want to store
    * cookies for head requests, only slows shit down
    */
  var emptyCookieStore: CookieStore = _
  /**
    * holds the HttpClient object for making requests
    */
  private val httpClient: HttpClient = initClient()


  def getHttpClient: HttpClient = httpClient

  /**
    * Makes an http fetch to go retrieve the HTML from a url, store it to disk and pass it off
    *
    * @param config Goose Configuration
    * @param url    The web address to fetch
    * @return If all goes well, a `Some[String]` otherwise `None`
    * @throws NotFoundException            (String)
    * @throws BadRequestException          (String)
    * @throws NotAuthorizedException       (String, Int)
    * @throws ServerErrorException         (String, Int)
    * @throws UnhandledStatusCodeException (String, Int)
    * @throws MaxBytesException            ()
    */
  def getHtml(config: Configuration, url: String): Option[String] = {
    var httpget: HttpGet = null
    var htmlResult: String = null
    var entity: HttpEntity = null
    var instream: InputStream = null

    // Identified the the apache http client does not drop URL fragments before opening the request to the host
    // more info: http://stackoverflow.com/questions/4251841/400-error-with-httpclient-for-a-link-with-an-anchor
    val cleanUrl = {
      val foundAt = url.indexOf("#")
      if (foundAt >= 0) url.substring(0, foundAt) else url
    }

    try {
      val localContext: HttpContext = new BasicHttpContext
      localContext.setAttribute(HttpClientContext.COOKIE_STORE, HtmlFetcher.emptyCookieStore)

      httpget = new HttpGet(cleanUrl)
      //      HttpProtocolParams.setUserAgent(httpClient.getParams, config.getBrowserUserAgent());
      //      val params = httpClient.getParams
      //      HttpConnectionParams.setConnectionTimeout(params, config.getConnectionTimeout())
      //      HttpConnectionParams.setSoTimeout(params, config.getSocketTimeout())
      val response: HttpResponse = httpClient.execute(httpget, localContext)

      HttpStatusValidator.validate(cleanUrl, response.getStatusLine.getStatusCode) match {
        case Left(ex) => throw ex
        case _ =>
      }

      entity = response.getEntity
      if (entity != null) {
        instream = entity.getContent
        var encodingType: String = config.getHtmlEncoding
        try {
          encodingType = ContentType.getOrDefault(entity).getCharset.displayName()
          if (encodingType == null) {
            encodingType = config.getDefaultHtmlEncoding
          }
        }
        catch {
          case e: Exception =>
            logger.trace("Unable to get charset for: " + cleanUrl)
            logger.trace("Encoding Type is: " + encodingType)
        }
        try {
          htmlResult = IOUtils.toString(instream, encodingType).trim
        }
        finally {
          EntityUtils.consume(entity)
        }
      }
      else {
        logger.trace("Unable to fetch URL Properly: " + cleanUrl)
      }
    }
    catch {
      case e: NullPointerException =>
        logger.warn(e.toString + " " + e.getMessage + " Caught for URL: " + cleanUrl)
      case e: MaxBytesException =>
        logger.trace("GRVBIGFAIL: " + cleanUrl + " Reached max bytes size")
        throw e
      case e: SocketException =>
        logger.warn(e.getMessage + " Caught for URL: " + cleanUrl)
      case e: SocketTimeoutException =>
        logger.trace(e.toString)
      case e: LoggableException =>
        logger.warn(e.getMessage)
        return None
      case e: Exception =>
        logger.trace("FAILURE FOR LINK: " + cleanUrl + " " + e.toString)
        return None
    }
    finally {
      if (instream != null) {
        try {
          instream.close()
        }
        catch {
          case e: Exception => logger.warn(e.getMessage + " Caught for URL: " + cleanUrl)
        }
      }
      if (httpget != null) {
        try {
          httpget.abort()
          entity = null
        }
        catch {
          case e: Exception => logger.warn(e.getMessage, e)
        }
      }
    }
    logger.debug("starting...")
    if (htmlResult == null || htmlResult.length < 1) {
      logger.debug("HTMLRESULT is empty or null")
      throw new NotHtmlException(cleanUrl)
    }
    var is: InputStream = null
    var mimeType: String = null
    try {
      is = new ByteArrayInputStream(htmlResult.getBytes("UTF-8"))
      mimeType = URLConnection.guessContentTypeFromStream(is)
      if (mimeType != null) {
        if (mimeType == "text/html" || mimeType == "application/xml") {
          return Some(htmlResult)
        }
        else {
          if (htmlResult.contains("<title>") && htmlResult.contains("<p>")) {
            return Some(htmlResult)
          }
          logger.trace("GRVBIGFAIL: " + mimeType + " - " + cleanUrl)
          throw new NotHtmlException(cleanUrl)
        }
      }
      else {
        throw new NotHtmlException(cleanUrl)
      }
    }
    catch {
      case e: UnsupportedEncodingException =>
        logger.warn(e.getMessage + " Caught for URL: " + cleanUrl)
      case e: IOException =>
        logger.warn(e.getMessage + " Caught for URL: " + cleanUrl)
    }
    None
  }

  private def initClient(): HttpClient = {
    logger.trace("Initializing HttpClient")

    emptyCookieStore = new CookieStore {
      def addCookie(cookie: Cookie): Unit = {
      }

      def getCookies: java.util.List[Cookie] = {
        emptyList
      }

      def clearExpired(date: Date): Boolean = {
        false
      }

      def clear(): Unit = {
      }

      private[network] var emptyList = new java.util.ArrayList[Cookie]
    }

    val requestConfig = RequestConfig.custom()
      .setCookieSpec(CookieSpecs.DEFAULT)
      .setConnectTimeout(10 * 1000)
      .setSocketTimeout(10 * 1000)
      .build()

    val socketConfig = SocketConfig.custom()
      .setTcpNoDelay(true)
      .build()

    val connManager = new PoolingHttpClientConnectionManager
    connManager.setMaxTotal(2000)
    connManager.setDefaultMaxPerRoute(500)

    val defaultHeaders = List(
      new BasicHeader(HttpHeaders.ACCEPT_LANGUAGE, "bg-bg,bg;q=0.8,en-us;q=0.5,en;q=0.3"),
      new BasicHeader(HttpHeaders.ACCEPT, "application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5"),
      new BasicHeader(HttpHeaders.CACHE_CONTROL, "max-age=0")).asJava


    HttpClients.custom()
      .setUserAgent("Mozilla/5.0 (X11; Linux i686; rv:50.0) Gecko/20100101 Firefox/50.0")
      .setDefaultRequestConfig(requestConfig)
      .setDefaultSocketConfig(socketConfig)
      .setConnectionManager(connManager)
      .setRetryHandler(new DefaultHttpRequestRetryHandler(0, false))
      .setDefaultHeaders(defaultHeaders)
      .build()

    //    httpParams.setParameter("http.protocol.content-charset", "UTF-8")
    //    httpParams.setParameter("http.connection.stalecheck", false)
    //    val params = httpClient.getParams.setParameter("http.conn-manager.timeout", 120000L)
    //      .setParameter("http.protocol.wait-for-continue", 10000L)
    //      .setParameter("http.tcp.nodelay", true)
  }
}


