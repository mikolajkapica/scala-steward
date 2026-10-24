/*
 * Copyright 2018-2025 Scala Steward contributors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.scalasteward.core.repoconfig

import cats.Eq
import cats.syntax.all.*
import io.circe.generic.semiauto.*
import io.circe.{Decoder, Encoder}
import org.scalasteward.core.data.{SemVer, Version}

final case class VersionPattern(
    prefix: Option[String] = None,
    suffix: Option[String] = None,
    exact: Option[String] = None,
    contains: Option[String] = None,
    change: Option[SemVer.Change] = None
) {
  def matches(update: Version.Update): Boolean = {
    val version = update.nextVersion.value
    prefix.forall(version.startsWith) &&
    suffix.forall(version.endsWith) &&
    exact.forall(_ === version) &&
    contains.forall(version.contains) &&
    change.forall(update.changeEarly.contains)
  }
}

object VersionPattern {
  implicit val versionPatternEq: Eq[VersionPattern] =
    Eq.fromUniversalEquals

  implicit val versionPatternDecoder: Decoder[VersionPattern] =
    Decoder[String]
      .map { s =>
        SemVer.Change.from(s) match {
          case Right(change) => VersionPattern(change = Some(change))
          case Left(_)       => VersionPattern(prefix = Some(s))
        }
      }
      .or(deriveDecoder[VersionPattern])

  implicit val versionPatternEncoder: Encoder[VersionPattern] = {
    val vpEncoder = deriveEncoder[VersionPattern]
    Encoder.instance {
      case VersionPattern(Some(prefix), None, None, None, None) => Encoder[String].apply(prefix)
      case VersionPattern(None, None, None, None, Some(change)) =>
        Encoder[SemVer.Change].apply(change)
      case vp => vpEncoder(vp)
    }
  }
}
