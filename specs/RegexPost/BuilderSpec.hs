
module RegexPost.BuilderSpec where


import Test.Hspec
import Test.QuickCheck

import RegexPost


spec :: Spec
spec = do
    describe "opt" $
        it "should combine a regex with empty." $
            opt (regex 'c') `shouldBe` ReOpt (ReLiteral 'c')

    describe "more1" $
        it "should combine a regex with concat and star." $
            more1 (regex 'c') `shouldBe` ReConcat (ReLiteral 'c') (ReStar (ReLiteral 'c'))

    describe "charClass" $ do
        it "should combine a single character with literal, alt, and fail." $
            charClass "c" `shouldBe` ReLiteral 'c'
        it "should combine multiple characters in a right-associative way." $
            charClass "cd" `shouldBe` ReAlt (ReLiteral 'c') (ReLiteral 'd')

    describe "(.+.)" $ do
        it "should have higher precedence than .|." $
            eg0 `shouldBe` ReAlt (ReConcat (ReLiteral 'a') (ReLiteral 'b'))
                                 (ReConcat (ReLiteral 'c') (ReStar (ReLiteral 'd')))
        it "should be right associative." $
            eg `shouldBe` ReConcat (ReConcat (ReLiteral '0') (ReStar (ReLiteral '0')))
                                   (ReConcat (ReLiteral '.')
                                             (ReConcat (ReLiteral '0') (ReLiteral '0')))
            where eg = more1 (regex '0') .+. regex '.' .+. regex '0' .+. regex '0'

