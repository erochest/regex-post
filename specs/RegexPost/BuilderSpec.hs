
module RegexPost.BuilderSpec where


import Test.Hspec
import Test.QuickCheck

import RegexPost


spec :: Spec
spec = do
    describe "opt" $
        it "should combine a regex with empty." $
            opt (re 'c') `shouldBe` ReAlt (ReLiteral 'c') ReEmpty

    describe "more1" $
        it "should combine a regex with concat and star." $
            more1 (re 'c') `shouldBe` ReConcat (ReLiteral 'c') (ReStar (ReLiteral 'c'))

    describe "charClass" $ do
        it "should combine a single character with literal, alt, and fail." $
            charClass "c" `shouldBe` ReAlt (ReLiteral 'c') ReFail
        it "should combine multiple characters in a right-associative way." $
            charClass "cd" `shouldBe` ReAlt (ReLiteral 'c') (ReAlt (ReLiteral 'd') ReFail)

    describe "(.+.)" $ do
        it "should have higher precedence than .|." $
            eg0 `shouldBe` ReAlt (ReConcat (ReLiteral 'a') (ReLiteral 'b'))
                                 (ReConcat (ReLiteral 'c') (ReStar (ReLiteral 'd')))
        it "should be right associative." $
            eg `shouldBe` ReConcat (ReConcat (ReLiteral '0') (ReStar (ReLiteral '0')))
                                   (ReConcat (ReLiteral '.')
                                             (ReConcat (ReLiteral '0') (ReLiteral '0')))
            where eg = more1 (re '0') .+. re '.' .+. re '0' .+. re '0'

