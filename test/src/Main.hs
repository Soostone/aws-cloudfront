module Main where


-------------------------------------------------------------------------------
import           Test.Tasty
-------------------------------------------------------------------------------
import qualified Aws.CloudFront.Tests.Commands.CreateInvalidationRequest
import qualified Aws.CloudFront.Tests.Commands.GetDistributionList
import qualified Aws.CloudFront.Tests.Commands.GetInvalidationList
import qualified Aws.CloudFront.Tests.Core
import qualified Aws.CloudFront.Tests.Types
import qualified Aws.CloudFront.Tests.Util
-------------------------------------------------------------------------------

main :: IO ()
main = defaultMain $ testGroup "aws-cloudfront"
  [ Aws.CloudFront.Tests.Commands.CreateInvalidationRequest.tests
  , Aws.CloudFront.Tests.Commands.GetDistributionList.tests
  , Aws.CloudFront.Tests.Commands.GetInvalidationList.tests
  , Aws.CloudFront.Tests.Core.tests
  , Aws.CloudFront.Tests.Types.tests
  , Aws.CloudFront.Tests.Util.tests
  ]
