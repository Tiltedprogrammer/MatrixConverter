module QTreeOps where

import QTree


nnz m = case m of
     QVal _ -> 1
     QNode tl tr bl br -> (nnz tl) + (nnz tr) + (nnz bl) + (nnz br)
     QError -> error "QError encountered"
     _ -> 0

mapQ isZ f m = 
    case m of
         QError -> QError
         QNone -> QNone
         QVal v -> (mkQNode isZ (f v))
         (QNode q1 q2 q3 q4) -> (reduceTree 
                                     (mapQ isZ f q1) 
                                     (mapQ isZ f q2) 
                                     (mapQ isZ f q3) 
                                     (mapQ isZ f q4));
                      
mkQNode isZ x = case (isZ x) of True -> QNone
                                False -> (QVal x) 

reduceTree n1 n2 n3 n4 = (QNode n1 n2 n3 n4)

eWiseAdd isZ g m1 m2 = 
    case m1 of
         QError -> QError
         QNone -> (m2)
         (QVal v1) -> (case m2 of 
                        QError -> QError 
                        QNone -> m1 
                        (QVal v) -> (mkQNode isZ (g v1 v))
                        (QNode t1 t2 t3 t4) -> QError)
         (QNode q1 q2 q3 q4) -> (case m2 of 
                                    QError -> QError
                                    QNone -> m1 
                                    (QVal v) -> QError 
                                    (QNode t1 t2 t3 t4) -> (reduceTree 
                                                                    (eWiseAdd isZ g q1 t1) 
                                                                    (eWiseAdd isZ g q2 t2)
                                                                    (eWiseAdd isZ g q3 t3) 
                                                                    (eWiseAdd isZ g q4 t4)))

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
                                                                    (eWiseMult g q4 t4);}}

kron isZ g m1 m2 =
    case m1 of 
         QError -> QError
         QNone -> QNone
         (QVal v) -> (mapQ isZ (g v) m2)
         (QNode q1 q2 q3 q4) -> (reduceTree 
                                      (kron isZ g q1 m2) 
                                      (kron isZ g q2 m2) 
                                      (kron isZ g q3 m2) 
                                      (kron isZ g q4 m2))

;


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