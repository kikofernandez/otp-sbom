[#assign noticeCategoryName = "include-in-notice-file"]
[#-- Add the licenses of the projects. --]
[#if projects?has_content]
This project contains or depends on third-party software components pursuant to the following licenses:
----
    [#assign isFirst = true]
    [#--
        Merge the licenses and copyrights of all projects and packages into a single list. The licenses are filtered
        using LicenseView.CONCLUDED_OR_DECLARED_AND_DETECTED. This is the default view which ignores declared and
        detected licenses if a license conclusion for a package was made. For projects this is the same as
        LicenseView.ALL, because projects cannot have concluded licenses. If copyrights were detected for a concluded
        license those statements are kept. Also filter all licenses that are configured not to be included in notice
        files.
    --]
    [#assign mergedLicenses = helper.filterForCategory(
        helper.mergeLicenses(projects + packages, LicenseView.CONCLUDED_OR_DECLARED_AND_DETECTED),
        noticeCategoryName
    )]
    [#list mergedLicenses as resolvedLicense]
        [#assign licenseName = resolvedLicense.license.simpleLicense()]
        [#assign licenseText = licenseTextProvider.getLicenseText(licenseName)!]
        [#assign locations = resolvedLicense.locations]


        [#assign paths = resolvedLicense.locations?map(resolvedLicenseLocation -> resolvedLicenseLocation.location.path)]
        [#assign uniquePaths = [] ]
        [#list paths as path]
            [#if ! uniquePaths?seq_contains(path)]
                [#assign uniquePaths = uniquePaths + [path] ]
            [/#if]
        [/#list]
        [#assign uniquePaths = uniquePaths?sort]


        [#if !licenseText?has_content][#continue][/#if]
        [#if isFirst]
            [#assign isFirst = false]
        [#else]
----
        [/#if]
        [#assign copyrights = resolvedLicense.getCopyrights()]
* Info:
  [#if resolvedLicense.license.exception()?has_content]
  * SPDX-License-Identifier: ${resolvedLicense.license.simpleLicense()} WITH ${resolvedLicense.license.exception()!}
  [#else]
  * SPDX-License-Identifier: ${resolvedLicense.license.simpleLicense()}
  [/#if]
  [#if resolvedLicense.license.getLicenseUrl()?has_content ]
  * License URL: ${resolvedLicense.license.getLicenseUrl()!}
  [/#if]
  * OTP Location:
  [#list uniquePaths as path]
      - ${path}
  [/#list]

  [#if copyrights?has_content ]
  * COPYRIGHTS
    %CopyrightBegin%
    [#list copyrights as c]
      - ${c}
    [/#list]
    %CopyrightEnd%
  [/#if]

        [#assign exceptionName = resolvedLicense.license.exception()!]
        [#assign exceptionText = licenseTextProvider.getLicenseText(exceptionName)!]
        [#if exceptionText?has_content]
  ${exceptionText}
        [/#if]
    [/#list]
[/#if]
