package org.scalasteward.core.vcs

import cats.effect.IO
import org.http4s.HttpRoutes
import org.http4s.client.Client
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.scalasteward.core.TestInstances.ioLogger
import org.scalasteward.core.TestSyntax._
import org.scalasteward.core.application.{Config, SupportedVCS}
import org.scalasteward.core.data.{ReleaseRelatedUrl, Update}
import org.scalasteward.core.mock.MockContext
import org.scalasteward.core.util.{HttpExistenceClient, Nel}
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers

class VCSExtraAlgTest extends AnyFunSuite with Matchers {
  val routes: HttpRoutes[IO] =
    HttpRoutes.of[IO] {
      case HEAD -> Root / "foo" / "bar" / "compare" / "v0.1.0...v0.2.0" => Ok("exist")
      case HEAD -> Root / "foo" / "buz" / "compare" / "v0.1.0...v0.2.0" => PermanentRedirect()
      case _                                                            => NotFound()
    }

  implicit val cfg = MockContext.config
  implicit val client = Client.fromHttpApp[IO](routes.orNotFound)
  implicit val httpExistenceClient =
    HttpExistenceClient.create[IO].allocated.map(_._1).unsafeRunSync()
  val vcsExtraAlg = VCSExtraAlg.create[IO]

  val updateFoo = Update.Single("com.example" % "foo" % "0.1.0", Nel.of("0.2.0"))
  val updateBar = Update.Single("com.example" % "bar" % "0.1.0", Nel.of("0.2.0"))
  val updateBuz = Update.Single("com.example" % "buz" % "0.1.0", Nel.of("0.2.0"))

  val update: Update.Single =
    Update.Single("ch.qos.logback" % "logback-classic" % "1.2.0", Nel.of("1.2.3"))

  test("getBranchCompareUrl: std vsc") {
    vcsExtraAlg
      .getReleaseRelatedUrls(uri"https://github.com/foo/foo", updateFoo)
      .unsafeRunSync() shouldBe List.empty

    vcsExtraAlg
      .getReleaseRelatedUrls(uri"https://github.com/foo/bar", updateBar)
      .unsafeRunSync() shouldBe List(
      ReleaseRelatedUrl.VersionDiff(uri"https://github.com/foo/bar/compare/v0.1.0...v0.2.0")
    )

    vcsExtraAlg
      .getReleaseRelatedUrls(uri"https://github.com/foo/buz", updateBuz)
      .unsafeRunSync() shouldBe List.empty
  }

  test("getBranchCompareUrl: github on prem") {
    implicit val cfg: Config = MockContext.config.copy(
      vcsType = SupportedVCS.GitHub,
      vcsApiHost = uri"https://github.on-prem.com/"
    )
    val githubOnPremVcsExtraAlg = VCSExtraAlg.create[IO]

    githubOnPremVcsExtraAlg
      .getReleaseRelatedUrls(uri"https://github.on-prem.com/foo/foo", updateFoo)
      .unsafeRunSync() shouldBe List.empty

    githubOnPremVcsExtraAlg
      .getReleaseRelatedUrls(uri"https://github.on-prem.com/foo/bar", updateBar)
      .unsafeRunSync() shouldBe List(
      ReleaseRelatedUrl.VersionDiff(uri"https://github.on-prem.com/foo/bar/compare/v0.1.0...v0.2.0")
    )

    githubOnPremVcsExtraAlg
      .getReleaseRelatedUrls(uri"https://github.on-prem.com/foo/buz", updateFoo)
      .unsafeRunSync() shouldBe List.empty
  }

  test("possibleCompareUrls") {
    val onPremVCS = "https://github.onprem.io/"
    val onPremVCSUri = uri"https://github.onprem.io/"

    VCSExtraAlg
      .possibleCompareUrls(
        SupportedVCS.GitHub,
        onPremVCSUri,
        uri"https://github.com/foo/bar",
        update
      )
      .map(_.url.renderString) shouldBe List(
      "https://github.com/foo/bar/compare/v1.2.0...v1.2.3",
      "https://github.com/foo/bar/compare/1.2.0...1.2.3",
      "https://github.com/foo/bar/compare/release-1.2.0...release-1.2.3"
    )
    // should canonicalize (drop last slash)
    VCSExtraAlg
      .possibleCompareUrls(
        SupportedVCS.GitHub,
        onPremVCSUri,
        uri"https://github.com/foo/bar/",
        update
      )
      .map(_.url.renderString) shouldBe List(
      "https://github.com/foo/bar/compare/v1.2.0...v1.2.3",
      "https://github.com/foo/bar/compare/1.2.0...1.2.3",
      "https://github.com/foo/bar/compare/release-1.2.0...release-1.2.3"
    )

    VCSExtraAlg
      .possibleCompareUrls(
        SupportedVCS.GitHub,
        onPremVCSUri,
        uri"https://gitlab.com/foo/bar",
        update
      )
      .map(_.url.renderString) shouldBe List(
      "https://gitlab.com/foo/bar/compare/v1.2.0...v1.2.3",
      "https://gitlab.com/foo/bar/compare/1.2.0...1.2.3",
      "https://gitlab.com/foo/bar/compare/release-1.2.0...release-1.2.3"
    )
    VCSExtraAlg
      .possibleCompareUrls(
        SupportedVCS.GitHub,
        onPremVCSUri,
        uri"https://bitbucket.org/foo/bar",
        update
      )
      .map(_.url.renderString) shouldBe List(
      "https://bitbucket.org/foo/bar/compare/v1.2.3..v1.2.0#diff",
      "https://bitbucket.org/foo/bar/compare/1.2.3..1.2.0#diff",
      "https://bitbucket.org/foo/bar/compare/release-1.2.3..release-1.2.0#diff"
    )

    VCSExtraAlg.possibleCompareUrls(
      SupportedVCS.GitHub,
      onPremVCSUri,
      uri"https://scalacenter.github.io/scalafix/",
      update
    ) shouldBe List.empty

    VCSExtraAlg
      .possibleCompareUrls(
        SupportedVCS.GitHub,
        onPremVCSUri,
        onPremVCSUri.addPath("/foo/bar"),
        update
      )
      .map(_.url.renderString) shouldBe List(
      s"${onPremVCS}foo/bar/compare/v1.2.0...v1.2.3",
      s"${onPremVCS}foo/bar/compare/1.2.0...1.2.3",
      s"${onPremVCS}foo/bar/compare/release-1.2.0...release-1.2.3"
    )
  }

  test("possibleChangelogUrls: github.com") {
    VCSExtraAlg
      .possibleReleaseRelatedUrls(
        SupportedVCS.GitHub,
        uri"https://github.com",
        uri"https://github.com/foo/bar",
        update
      )
      .map(_.url.renderString) shouldBe List(
      "https://github.com/foo/bar/releases/tag/v1.2.3",
      "https://github.com/foo/bar/releases/tag/1.2.3",
      "https://github.com/foo/bar/releases/tag/release-1.2.3",
      "https://github.com/foo/bar/blob/master/ReleaseNotes.md",
      "https://github.com/foo/bar/blob/master/ReleaseNotes.markdown",
      "https://github.com/foo/bar/blob/master/ReleaseNotes.rst",
      "https://github.com/foo/bar/blob/master/RELEASES.md",
      "https://github.com/foo/bar/blob/master/RELEASES.markdown",
      "https://github.com/foo/bar/blob/master/RELEASES.rst",
      "https://github.com/foo/bar/blob/master/Releases.md",
      "https://github.com/foo/bar/blob/master/Releases.markdown",
      "https://github.com/foo/bar/blob/master/Releases.rst",
      "https://github.com/foo/bar/blob/master/releases.md",
      "https://github.com/foo/bar/blob/master/releases.markdown",
      "https://github.com/foo/bar/blob/master/releases.rst",
      "https://github.com/foo/bar/blob/master/CHANGELOG.md",
      "https://github.com/foo/bar/blob/master/CHANGELOG.markdown",
      "https://github.com/foo/bar/blob/master/CHANGELOG.rst",
      "https://github.com/foo/bar/blob/master/Changelog.md",
      "https://github.com/foo/bar/blob/master/Changelog.markdown",
      "https://github.com/foo/bar/blob/master/Changelog.rst",
      "https://github.com/foo/bar/blob/master/changelog.md",
      "https://github.com/foo/bar/blob/master/changelog.markdown",
      "https://github.com/foo/bar/blob/master/changelog.rst",
      "https://github.com/foo/bar/blob/master/CHANGES.md",
      "https://github.com/foo/bar/blob/master/CHANGES.markdown",
      "https://github.com/foo/bar/blob/master/CHANGES.rst",
      "https://github.com/foo/bar/compare/v1.2.0...v1.2.3",
      "https://github.com/foo/bar/compare/1.2.0...1.2.3",
      "https://github.com/foo/bar/compare/release-1.2.0...release-1.2.3"
    )
  }

  test("possibleChangelogUrls: gitlab.com") {
    VCSExtraAlg
      .possibleReleaseRelatedUrls(
        SupportedVCS.GitHub,
        uri"https://github.com",
        uri"https://gitlab.com/foo/bar",
        update
      )
      .map(_.url.renderString) shouldBe
      VCSExtraAlg.possibleReleaseNotesFilenames.map(name =>
        s"https://gitlab.com/foo/bar/blob/master/$name"
      ) ++
      VCSExtraAlg.possibleChangelogFilenames.map(name =>
        s"https://gitlab.com/foo/bar/blob/master/$name"
      ) ++
      List(
        "https://gitlab.com/foo/bar/compare/v1.2.0...v1.2.3",
        "https://gitlab.com/foo/bar/compare/1.2.0...1.2.3",
        "https://gitlab.com/foo/bar/compare/release-1.2.0...release-1.2.3"
      )
  }

  test("possibleChangelogUrls: on-prem gitlab") {
    VCSExtraAlg
      .possibleReleaseRelatedUrls(
        SupportedVCS.Gitlab,
        uri"https://gitlab.on-prem.net",
        uri"https://gitlab.on-prem.net/foo/bar",
        update
      )
      .map(_.url.renderString) shouldBe
      VCSExtraAlg.possibleReleaseNotesFilenames.map(name =>
        s"https://gitlab.on-prem.net/foo/bar/blob/master/$name"
      ) ++
      VCSExtraAlg.possibleChangelogFilenames.map(name =>
        s"https://gitlab.on-prem.net/foo/bar/blob/master/$name"
      ) ++
      List(
        "https://gitlab.on-prem.net/foo/bar/compare/v1.2.0...v1.2.3",
        "https://gitlab.on-prem.net/foo/bar/compare/1.2.0...1.2.3",
        "https://gitlab.on-prem.net/foo/bar/compare/release-1.2.0...release-1.2.3"
      )
  }

  test("possibleChangelogUrls: bitbucket.org") {
    VCSExtraAlg
      .possibleReleaseRelatedUrls(
        SupportedVCS.GitHub,
        uri"https://github.com",
        uri"https://bitbucket.org/foo/bar",
        update
      )
      .map(_.url.renderString) shouldBe
      VCSExtraAlg.possibleReleaseNotesFilenames.map(name =>
        s"https://bitbucket.org/foo/bar/master/$name"
      ) ++
      VCSExtraAlg.possibleChangelogFilenames.map(name =>
        s"https://bitbucket.org/foo/bar/master/$name"
      ) ++
      List(
        "https://bitbucket.org/foo/bar/compare/v1.2.3..v1.2.0#diff",
        "https://bitbucket.org/foo/bar/compare/1.2.3..1.2.0#diff",
        "https://bitbucket.org/foo/bar/compare/release-1.2.3..release-1.2.0#diff"
      )
  }

  test("possibleChangelogUrls: homepage") {
    VCSExtraAlg
      .possibleReleaseRelatedUrls(
        SupportedVCS.GitHub,
        uri"https://github.com",
        uri"https://scalacenter.github.io/scalafix/",
        update
      )
      .map(_.url.renderString) shouldBe List.empty
  }
}
