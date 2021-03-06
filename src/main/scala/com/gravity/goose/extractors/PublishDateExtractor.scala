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
package com.gravity.goose.extractors

import java.util.Date
import javax.xml.datatype.DatatypeFactory

import com.typesafe.scalalogging.StrictLogging
import org.jsoup.nodes.Element

import scala.util.Try
import scala.util.control.NonFatal

/**
  * Implement this class to extract the [[Date]] of when this article was published.
  */
/**
  * Created by IntelliJ IDEA.
  * User: robbie
  * Date: 5/19/11
  * Time: 2:50 PM
  */
abstract class PublishDateExtractor extends Extractor[Date] {
  /**
    * Intended to search the DOM and identify the [[Date]] of when this article was published.
    * <p>This will be called by the [[ContentExtractor#extractContent(String)]] method and will be passed to [[com.gravity.goose.Article#setPublishDate(java.util.Date)]]</p>
    *
    * @param rootElement passed in from the [[ContentExtractor]] after the article has been parsed
    * @return [[Date]] of when this particular article was published or <code>null</code> if no date could be found.
    */
  def extract(rootElement: Element): Date
}

object PublishDateExtractor extends StrictLogging {
  val logPrefix = "PublishDateExtractor: "

  lazy val datatypeFactory: DatatypeFactory = DatatypeFactory.newInstance()

  /**
    * Helper function to return the minimum of two non-null Java Dates.
    */
  def minDate(lhs: java.util.Date, rhs: java.util.Date): java.util.Date = {
    if (lhs.getTime < rhs.getTime) lhs
    else rhs
  }

  /**
    * Helper function to parse ISO 8601 date/time strings safely.
    */
  def safeParseISO8601Date(txt: String): Option[java.util.Date] = {
    if (txt == null || txt.isEmpty)
      return None

    val t = Try(datatypeFactory.newXMLGregorianCalendar(txt).toGregorianCalendar.getTime)
    t.failed.foreach {
      case NonFatal(ex) =>
        logger.info(s"`$txt` could not be parsed to date as it did not meet the ISO 8601 spec", ex)
    }
    t.toOption
  }
}

