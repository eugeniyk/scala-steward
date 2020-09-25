/*
 * Copyright 2018-2020 Scala Steward contributors
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

package org.scalasteward.core.vcs

import cats.Monad
import cats.syntax.all._
import org.http4s.Uri
import org.scalasteward.core.application.SupportedVCS.{Bitbucket, BitbucketServer, GitHub, Gitlab}
import org.scalasteward.core.application.{Config, SupportedVCS}
import org.scalasteward.core.data.ReleaseRelatedUrl.VersionDiff
import org.scalasteward.core.data.{ReleaseRelatedUrl, Update}
import org.scalasteward.core.util.HttpExistenceClient

trait VCSExtraAlg[F[_]] {
  def getReleaseRelatedUrls(repoUrl: Uri, update: Update): F[List[ReleaseRelatedUrl]]
}

object VCSExtraAlg {
  def create[F[_]](implicit
      existenceClient: HttpExistenceClient[F],
      config: Config,
      F: Monad[F]
  ): VCSExtraAlg[F] =
    new VCSExtraAlg[F] {
      override def getReleaseRelatedUrls(repoUrl: Uri, update: Update): F[List[ReleaseRelatedUrl]] =
        possibleReleaseRelatedUrls(config.vcsType, config.vcsApiHost, repoUrl, update)
          .filterA(releaseRelatedUrl => existenceClient.exists(releaseRelatedUrl.url))
    }

  private[vcs] def possibleReleaseRelatedUrls(
      vcsType: SupportedVCS,
      vcsUri: Uri,
      repoUrl: Uri,
      update: Update
  ): List[ReleaseRelatedUrl] = {
    val repoVCSType = extractRepoVCSType(vcsType, vcsUri, repoUrl)

    val github = repoVCSType
      .collect { case GitHub =>
        possibleTags(update.nextVersion).map(tag =>
          ReleaseRelatedUrl.GitHubReleaseNotes(repoUrl / "releases" / "tag" / tag)
        )
      }
      .getOrElse(List.empty)

    def files(fileNames: List[String]): List[Uri] = {
      val maybeSegments = repoVCSType.map {
        case SupportedVCS.GitHub | SupportedVCS.Gitlab             => List("blob", "master")
        case SupportedVCS.Bitbucket | SupportedVCS.BitbucketServer => List("master")
      }

      maybeSegments.toList.flatMap { segments =>
        val base = segments.foldLeft(repoUrl)(_ / _)
        fileNames.map(name => base / name)
      }
    }

    val customChangelog = files(possibleChangelogFilenames).map(ReleaseRelatedUrl.CustomChangelog)
    val customReleaseNotes =
      files(possibleReleaseNotesFilenames).map(ReleaseRelatedUrl.CustomReleaseNotes)

    github ++ customReleaseNotes ++ customChangelog ++ possibleCompareUrls(
      vcsType,
      vcsUri,
      repoUrl,
      update
    )
  }

  private[vcs] def possibleCompareUrls(
      vcsType: SupportedVCS,
      vcsUri: Uri,
      repoUrl: Uri,
      update: Update
  ): List[VersionDiff] = {
    val from = update.currentVersion
    val to = update.nextVersion

    extractRepoVCSType(vcsType, vcsUri, repoUrl)
      .map {
        case GitHub | Gitlab =>
          possibleTags(from).zip(possibleTags(to)).map { case (from1, to1) =>
            VersionDiff(repoUrl / "compare" / s"$from1...$to1")
          }
        case Bitbucket | BitbucketServer =>
          possibleTags(from).zip(possibleTags(to)).map { case (from1, to1) =>
            VersionDiff((repoUrl / "compare" / s"$to1..$from1").withFragment("diff"))
          }
      }
      .getOrElse(List.empty)
  }

  private def extractRepoVCSType(
      vcsType: SupportedVCS,
      vcsUri: Uri,
      repoUrl: Uri
  ): Option[SupportedVCS] = {
    val host = repoUrl.host.map(_.value)
    if (vcsUri.host.map(_.value).contains(host.getOrElse("")))
      Option(vcsType)
    else
      host
        .collect {
          case "github.com" => GitHub
          case "gitlab.com" => Gitlab
        }
        .orElse {
          if (host.contains_("bitbucket.org"))
            Some(Bitbucket)
          else None
        }
  }

  private[vcs] val possibleChangelogFilenames: List[String] = {
    val baseNames = List(
      "CHANGELOG",
      "Changelog",
      "changelog",
      "CHANGES"
    )
    possibleFilenames(baseNames)
  }

  private[vcs] val possibleReleaseNotesFilenames: List[String] = {
    val baseNames = List(
      "ReleaseNotes",
      "RELEASES",
      "Releases",
      "releases"
    )
    possibleFilenames(baseNames)
  }

  private[vcs] def possibleFilenames(baseNames: List[String]): List[String] = {
    val extensions = List("md", "markdown", "rst")
    (baseNames, extensions).mapN { case (base, ext) => s"$base.$ext" }
  }

  private def possibleTags(version: String): List[String] =
    List(s"v$version", version, s"release-$version")
}
