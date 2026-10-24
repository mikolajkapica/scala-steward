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

import cats.syntax.all.*
import io.circe.Json
import io.circe.syntax.*
import munit.FunSuite
import org.scalasteward.core.data.{SemVer, Version}

class VersionPatternTest extends FunSuite {
  test("decode a change type from a plain string") {
    assertEquals(
      Json.fromString("minor").as[VersionPattern],
      Right(VersionPattern(change = Some(SemVer.Change.Minor)))
    )
  }

  test("decode any other plain string as a prefix") {
    assertEquals(
      Json.fromString("1.1.").as[VersionPattern],
      Right(VersionPattern(prefix = Some("1.1.")))
    )
  }

  test("decode a change type combined with a prefix") {
    val json = Json.obj("prefix" -> "2.".asJson, "change" -> "patch".asJson)
    assertEquals(
      json.as[VersionPattern],
      Right(VersionPattern(prefix = Some("2."), change = Some(SemVer.Change.Patch)))
    )
  }

  test("encode a change-only pattern as a plain string") {
    assertEquals(
      VersionPattern(change = Some(SemVer.Change.Major)).asJson,
      Json.fromString("major")
    )
  }

  test("round-trip patterns") {
    List(
      VersionPattern(prefix = Some("1.1.")),
      VersionPattern(prefix = Some("major")),
      VersionPattern(change = Some(SemVer.Change.PreRelease)),
      VersionPattern(prefix = Some("2."), change = Some(SemVer.Change.Minor)),
      VersionPattern(suffix = Some("jre8"))
    ).foreach(vp => assertEquals(vp.asJson.as[VersionPattern], Right(vp)))
  }

  private def update(current: String, next: String): Version.Update =
    Version.Update(Version(current), Version(next))

  test("change matches the kind of update from the current version") {
    val minor = VersionPattern(change = Some(SemVer.Change.Minor))
    assert(minor.matches(update("1.2.3", "1.3.0")))
    assert(!minor.matches(update("1.2.3", "1.2.4")))
    assert(!minor.matches(update("1.2.3", "2.0.0")))
  }

  test("change uses early semver for 0.x versions") {
    assert(VersionPattern(change = Some(SemVer.Change.Minor)).matches(update("0.23.33", "0.23.38")))
    assert(VersionPattern(change = Some(SemVer.Change.Major)).matches(update("0.23.38", "0.24.0")))
  }

  test("change classifies a release candidate to its final release as pre-release") {
    val preRelease = VersionPattern(change = Some(SemVer.Change.PreRelease))
    assert(preRelease.matches(update("1.2.3-RC1", "1.2.3")))
  }

  test("change never matches versions that aren't semver") {
    val patch = VersionPattern(change = Some(SemVer.Change.Patch))
    assert(!patch.matches(update("4.2.7.Final", "4.2.19.Final")))
  }

  test("change and prefix must both match") {
    val pattern = VersionPattern(prefix = Some("2."), change = Some(SemVer.Change.Minor))
    assert(pattern.matches(update("2.1.0", "2.2.0")))
    assert(!pattern.matches(update("1.1.0", "1.2.0")))
    assert(!pattern.matches(update("2.1.0", "2.1.1")))
  }

  test("an invalid change in the object form reports the allowed values") {
    val error = Json.obj("change" -> "Major".asJson).as[VersionPattern].leftMap(_.getMessage)
    assert(error.left.exists(_.contains("Allowed values are")), error)
  }
}
