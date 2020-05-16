module Api.Endpoints exposing (endpoints)

import Secrets exposing (tokenCircleCI)

endpoints : List String
endpoints =
    [ modularisationUrl,
    analyticsUrl,
    coreUrl,
    utilUrl,
    utilUrl,
    cleanupsUrl,
    deeplinksUrl,
    dependenciesUrl,
    formattersUrl,
    validatorsUrl,
    disposeByUrl,
    miscUrl,
    serviceManagerUrl,
    sessionUrl,
    modelsUrl,
    pricingUrl,
    widgetsUrl,
    errorsUrl,
    resourcesUrl ]

modularisationUrl : String
modularisationUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization",
        appendBuildSuffix
    ]

analyticsUrl : String
analyticsUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-analytics",
        appendBuildSuffix
    ]

coreUrl : String
coreUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-core",
        appendBuildSuffix
    ]

serviceManagerUrl : String
serviceManagerUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-service-manager",
        appendBuildSuffix
    ]

sessionUrl : String
sessionUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-session",
        appendBuildSuffix
    ]

miscUrl : String
miscUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-misc",
        appendBuildSuffix
    ]

deeplinksUrl : String
deeplinksUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-deeplinks",
        appendBuildSuffix
    ]

cleanupsUrl : String
cleanupsUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-cleanups",
        appendBuildSuffix
    ]

disposeByUrl : String
disposeByUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-disposeBy-removal",
        appendBuildSuffix
    ]

dependenciesUrl : String
dependenciesUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-dependencies",
        appendBuildSuffix
    ]

formattersUrl : String
formattersUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-formatters",
        appendBuildSuffix
    ]

validatorsUrl : String
validatorsUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-validators",
        appendBuildSuffix
    ]

utilUrl : String
utilUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-util",
        appendBuildSuffix
    ]

modelsUrl : String
modelsUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-models",
        appendBuildSuffix
    ]

pricingUrl : String
pricingUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-pricing",
        appendBuildSuffix
    ]

resourcesUrl : String
resourcesUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-resources",
        appendBuildSuffix
    ]

widgetsUrl : String
widgetsUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-widgets",
        appendBuildSuffix
    ]

errorsUrl : String
errorsUrl =
    String.concat[
        "https://circleci.com/api/v1.1/project/gh/undo-insurance/android/tree/modularization-errors",
        appendBuildSuffix
    ]

appendApiToken : String
appendApiToken = String.concat["?circle-token=", tokenCircleCI]

appendBuildSuffix : String
appendBuildSuffix = String.concat[appendApiToken, "&limit=50"]