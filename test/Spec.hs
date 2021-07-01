-- module QTreeSpec where is not needed for stack

import QTree
import QTreeOps
import Test.Hspec
import System.FilePath
import System.Directory




example1 :: IO (MatrixMarket Int)
example1 = do
    cd <- getCurrentDirectory
    readMtx $ cd </> "data" </> "exampleQTree2.mtx"

example2 :: IO (MatrixMarket Int)
example2 = do
    cd <- getCurrentDirectory
    readMtx $ cd </> "data" </> "exampleQTree3.mtx"

example3 :: IO (MatrixMarket Int)
example3  = do
    cd <- getCurrentDirectory
    readMtx $ cd </> "data" </> "exampleQTree4.mtx"

example4 :: IO (MatrixMarket Int)
example4  = do
    cd <- getCurrentDirectory
    readMtx $ cd </> "data" </> "exampleQTree5.mtx"




main :: IO ()
main = hspec $ do
        describe "matrixMarketToQTree" $ do
            mtxQ1 <- runIO example1
            it "exampleQTree2.mtx which is 4x4 matrix with entries at (0,0,1), (0,1,1), (1,0,5)" $
                matrixMarketToQTree mtxQ1 `shouldBe` QNode (QNode (QVal 1) (QVal 1) (QVal 5) QNone) QNone QNone QNone
            mtxQ2 <- runIO example2
            it "exampleQTree3.mtx which is 8x8 matrix with one entry at (8,8,322)" $
                matrixMarketToQTree mtxQ2 `shouldBe` QNode  QNone QNone QNone (QNode QNone
                                                                                     QNone
                                                                                     QNone
                                                                                     (QNode QNone QNone QNone (QVal 322))
                                                                               )
            mtxQ3 <- runIO example3
            it "exampleQTree4.mtx which is 8x8 diagonal matrix" $
                 matrixMarketToQTree mtxQ3 `shouldBe` QNode (QNode (QNode (QVal 1) QNone QNone (QVal 2))
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 3) QNone QNone (QVal 4)))
                                                             QNone
                                                             QNone
                                                             (QNode (QNode (QVal 5) QNone QNone (QVal 6))
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 7) QNone QNone (QVal 8)))
        describe "mask test" $ do
            mtxQ1 <- runIO example3
            it "mask test on 8x8 diagonal matrix" $
                 let mtxQ1Mask = MNode (MNode (MNode MVal MNone MNone MVal)
                                                                    MNone
                                                                    MNone
                                                                    (MNode MVal MNone MNone MVal))
                                        MNone
                                        MNone
                                        MNone in
                                            mask (matrixMarketToQTree mtxQ1) mtxQ1Mask `shouldBe` QNode (QNode (QNode (QVal 1) QNone QNone (QVal 2))
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 3) QNone QNone (QVal 4)))
                                                             QNone
                                                             QNone
                                                             QNone
        describe "scalarOp test" $ do
            mtxQ1 <- runIO example3
            it "scalarOp (\a -> 2 * a)" $
               eWizeScalarOp (*) 2 (matrixMarketToQTree mtxQ1) `shouldBe` QNode (QNode (QNode (QVal 2) QNone QNone (QVal 4))
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 6) QNone QNone (QVal 8)))
                                                             QNone
                                                             QNone
                                                             (QNode (QNode (QVal 10) QNone QNone (QVal 12))
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 14) QNone QNone (QVal 16)))
        describe "eWiseAdd test" $ do
            mtxQ1 <- runIO example3
            it "eWiseAdd example3 example3" $ let m = matrixMarketToQTree mtxQ1 in
                eWiseAdd (+) m m
                 `shouldBe` QNode (QNode (QNode (QVal 2) QNone QNone (QVal 4))
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 6) QNone QNone (QVal 8)))
                                                             QNone
                                                             QNone
                                                             (QNode (QNode (QVal 10) QNone QNone (QVal 12))
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 14) QNone QNone (QVal 16)))
        describe "eWiseMult test" $ do
            mtxQ1 <- runIO example3
            mtxQ2 <- runIO example4
            it "eWiseMult example3 example4" $ let m1 = matrixMarketToQTree mtxQ1
                                                   m2 = matrixMarketToQTree mtxQ2 in
                eWiseMult (*) m1 m2
                 `shouldBe` QNode (QNode (QNode (QVal 1) QNone QNone (QVal 4))
                                                                    QNone
                                                                    QNone
                                                                    QNone)
                                                             QNone
                                                             QNone
                                                             (QNode QNone
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 49) QNone QNone (QVal 64)))
        describe "mMult test" $ do
            mtxQ1 <- runIO example3
            mtxQ2 <- runIO example4
            it "mMult example3 example4" $ let m1 = matrixMarketToQTree mtxQ1
                                               m2 = matrixMarketToQTree mtxQ2 in
                mMult (+) (*) m1 m2
                 `shouldBe` QNode (QNode (QNode (QVal 1) QNone QNone (QVal 4))
                                                                    QNone
                                                                    QNone
                                                                    QNone)
                                                             QNone
                                                             QNone
                                                             (QNode QNone
                                                                    QNone
                                                                    QNone
                                                                    (QNode (QVal 49) QNone QNone (QVal 64))) 

        describe "qTreeToBoolTest" $ do
            mtxQ1 <- runIO example1
            it "qTreeToBool example1" $ let m1 = matrixMarketToQTree mtxQ1 in
                qTreeToBool m1 `shouldBe` QNode (QNode (QVal True) (QVal True) (QVal True) QNone) QNone QNone QNone