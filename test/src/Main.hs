module Main where


-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import qualified Aws.CloudFront.Tests.Core
import qualified Aws.CloudFront.Tests.Commands.CreateInvalidationRequest
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "aws-cloudfront"
  [ Aws.CloudFront.Tests.Commands.CreateInvalidationRequest.tests
  , Aws.CloudFront.Tests.Core.tests
  ]
