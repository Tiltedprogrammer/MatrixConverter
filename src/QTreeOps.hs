module QTreeOps where

import QTree


eWiseAdd = \g m1 m2 -> 
    case m1 of{
         QError -> QError;
         QNone -> case m2 of {QError -> QError; 
                             QNone -> QNone; 
                             QVal v -> m2; 
                             QNode t1 t2 t3 t4 -> m2;};
         QVal v1 -> case m2 of {QError -> QError; 
                               QNone -> m1; 
                               QVal v -> QVal (g v1 v); 
                               QNode t1 t2 t3 t4 -> QError;};
         QNode q1 q2 q3 q4 -> case m2 of {QError -> QError;
                                         QNone -> m1; 
                                         QVal v -> QError; 
                                         QNode t1 t2 t3 t4 -> QNode 
                                                                    (eWiseAdd g q1 t1) 
                                                                    (eWiseAdd g q2 t2) 
                                                                    (eWiseAdd g q3 t3) 
                                                                    (eWiseAdd g q4 t4);};
};

eWiseMult = \g m1 m2 -> 
    case m1 of{
         QError -> QError;
         QNone -> case m2 of {QError -> QError; 
                             QNone -> QNone; 
                             QVal v -> m1; 
                             QNode t1 t2 t3 t4 -> m1;};
         QVal v1 -> case m2 of {QError -> QError; 
                               QNone -> m2; 
                               QVal v -> QVal (g v1 v); 
                               QNode t1 t2 t3 t4 -> QError;};
         QNode q1 q2 q3 q4 -> case m2 of {QError -> QError;
                                         QNone -> m2; 
                                         QVal v -> QError; 
                                         QNode t1 t2 t3 t4 -> QNode 
                                                                    (eWiseMult g q1 t1) 
                                                                    (eWiseMult g q2 t2) 
                                                                    (eWiseMult g q3 t3) 
                                                                    (eWiseMult g q4 t4);};
};


mMult = \g h m1 m2 -> 
    case m1 of {
         QError -> QError;
         QNone -> QNone;
         QVal v1 -> 
             case m2 of {
                  QError -> QError; 
                  QNone -> QNone; 
                  QVal v -> QVal (h v1 v); 
                  QNode t1 t2 t3 t4 -> QError;
             };
         QNode q1 q2 q3 q4 -> 
              case m2 of {
                   QError -> QError;
                   QNone -> QNone; 
                   QVal v -> QError;
                   QNode t1 t2 t3 t4 -> QNode
                                               (eWiseAdd g (mMult g h q1 t1)(mMult g h q2 t3)) 
                                               (eWiseAdd g (mMult g h q1 t2)(mMult g h q2 t4)) 
                                               (eWiseAdd g (mMult g h q3 t1)(mMult g h q4 t3)) 
                                               (eWiseAdd g (mMult g h q3 t2)(mMult g h q4 t4));};
};

mask = \m msk ->
    case m of{
         QError -> QError;
         QNone -> QNone;
         QVal v1 -> case msk of {MQNone -> QNone; MQVal-> m;};
         QNode q1 q2 q3 q4 -> case msk of {
                                  MQNone -> QNone; 
                                  MQVal -> m; 
                                  MQNode t1 t2 t3 t4 -> 
                                   QNode  (mask q1 t1) (mask q2 t2) (mask q3 t3) (mask q4 t4);};
                      };
                      
eWizeScalarOp = \g s m -> 
    case m of{
         QError -> QError;
         QNone -> QNone;
         QVal v1 -> QVal (g v1 s);
         QNode q1 q2 q3 q4 -> QNode 
                                 (eWizeScalarOp g s q1) 
                                     (eWizeScalarOp g s q2) 
                                     (eWizeScalarOp g s q3) 
                                     (eWizeScalarOp g s q4); 
};           