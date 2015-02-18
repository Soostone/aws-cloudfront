# aws-cloudfront

Bindings for AWS CloudFront

# Status

Not at all ready for use.

## Supported Cloudfront API Calls

-   [-] Web Distributions <code>[2/6]</code>
    -   [ ] Create
    -   [X] Index
    -   [X] Show
    -   [ ] Get Config
    -   [ ] Update Config
    -   [ ] Delete Config
-   [ ] RTMP Distributions <code>[0/6]</code>
    -   [ ] Create
    -   [ ] Index
    -   [ ] Show
    -   [ ] Get Config
    -   [ ] Update Config
    -   [ ] Delete Config
-   [ ] Origin Access Identities <code>[0/6]</code>
    -   [ ] Create
    -   [ ] Index
    -   [ ] Show
    -   [ ] Get Config
    -   [ ] Update Config
    -   [ ] Delete Config
-   [X] Invalidations <code>[3/3]</code>
    -   [X] Create
    -   [X] Index
    -   [X] Show

# Tasks


## TODO Travis CI setup

## DONE Strictness analysis

## TODO Stronger error types

## TODO should we share some type instead of deriving our own for ObjectKey?

## TODO Note usage of S3OriginConfig

## TODO Use <?> to annotate all parsers

## TODO Doc header in all modules

## TODO README examples

## TODO Move types and parsers into Types module

## TODO Use fromString . show for toText on sum types

## TODO Break out parseDistributionSummary

## TODO reorg test modules

## TODO Be consistent about fields that are an ID, i.e. invInvalidationId -> invId

## TODO Not emitting request id or x-amz-id-2

## TODO Probably drop "Request" suffix from commands. unnecessary

## TODO Ensure not sending superfluous headers

# Acknowledgements

Much of the design queues for this project were derived by the
excellent [aws-sns](https://github.com/alephcloud/hs-aws-sns) package by AlephCloud Systems, Inc.
