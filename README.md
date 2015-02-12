# aws-cloudfront

Bindings for AWS CloudFront

# Status

Not at all ready for use.

## Supported Cloudfront API Calls

-   [-] Web Distributions <code>[0/6]</code>
    -   [ ] Create
    -   [-] Index
    -   [ ] Show
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
-   [-] Invalidations <code>[0/3]</code>
    -   [ ] Create
    -   [ ] Index
    -   [-] Show

# Tasks


## TODO Travis CI setup

## TODO Strictness analysis

## TODO Stronger error types

## TODO should we share some type instead of deriving our own for ObjectKey?

## TODO Note usage of S3OriginConfig

# Acknowledgements

Much of the design queues for this project were derived by the
excellent [aws-sns](https://github.com/alephcloud/hs-aws-sns) package by AlephCloud Systems, Inc.
