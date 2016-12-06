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

package com.gravity.goose.utils

import com.gravity.goose.text.{HashUtils, StringReplacement}
import java.net.{MalformedURLException, URI, URL}

import com.typesafe.scalalogging.StrictLogging
import org.apache.http.client.methods.HttpGet

/**
  * Created by Jim Plush
  * User: jim
  * Date: 8/14/11
  */

case class ParsingCandidate(urlString: String, linkhash: String, url: URL)

object URLHelper extends StrictLogging {

  private val ESCAPED_FRAGMENT_REPLACEMENT: StringReplacement = StringReplacement.compile("#!", "?_escaped_fragment_=")

  /**
    * returns a ParseCandidate object  that is a valid URL
    */
  def getCleanedUrl(urlToCrawl: String): Option[ParsingCandidate] = {

    val finalURL =
      if (urlToCrawl.contains("#!")) ESCAPED_FRAGMENT_REPLACEMENT.replaceAll(urlToCrawl) else urlToCrawl

    try {
      val url = new URL(finalURL)
      val linkhash = HashUtils.md5(finalURL)
      Some(ParsingCandidate(finalURL, linkhash, url))
    }
    catch {
      case e: MalformedURLException => {
        logger.warn(s"$urlToCrawl - is a malformed URL and cannot be processed")
        None
      }
      case unknown: Exception =>
        logger.error(s"Unable to process URL: ${urlToCrawl} due to an unexpected exception. Reason: ${unknown.getMessage}", urlToCrawl, unknown)
        None
    }
  }

  def tryToURL(url: String): Option[URL] = {
    val finalUrl = if (url.contains("#!")) {
      ESCAPED_FRAGMENT_REPLACEMENT.replaceAll(url)
    } else {
      url
    }

    try {
      Some(new URL(finalUrl))
    } catch {
      case _: Exception => None
    }
  }

  def tryToURI(url: String): Option[URI] = {
    val finalUrl = if (url.contains("#!")) {
      ESCAPED_FRAGMENT_REPLACEMENT.replaceAll(url)
    } else {
      url
    }

    try {
      Some(URI.create(finalUrl))
    } catch {
      case _: Exception => None
    }
  }

  def tryToHttpGet(url: String): Option[HttpGet] = {
    tryToURI(url) match {
      case Some(uri) => Some(new HttpGet(uri))
      case None => None
    }
  }
}