module EWiseAddChainOut2 where


data QTree a = QNone  | QVal a | QNode (QTree a) (QTree a) (QTree a) (QTree a) | QError ;
data MaskQTree  = MNone  | MVal  | MNode MaskQTree MaskQTree MaskQTree MaskQTree;

pls = (+)

mlt = (*)

m1 = QNode (QNode (QNode (QVal 1) QNone QNone (QVal 2)) QNone QNone (QNode (QVal 3) QNone QNone (QVal 4))) QNone QNone (QNode (QNode (QVal 5) QNone QNone (QVal 6)) QNone QNone (QNode (QVal
      7) QNone QNone (QVal 8)))

-- m2 m3 are multiplied
m2 = m1

m3 = QNode (QNode (QNode (QVal 1) QNone QNone (QVal 2)) QNone QNone QNone) QNone QNone (QNode QNone QNone QNone (QNode (QVal 7) QNone QNone (QVal 8)))




main = let
  f=(\x->
    (\y->
      (\z->
        case  x  of {
          QError  -> QError;
          QNone  ->
            case  y  of {
              QError  -> QError;
              QNone  ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> QNone;
                  QVal x1143 -> QNone;
                  QNode x1144 x1145 x1146 x1147 -> QNone;
                };
              QVal x1118 ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> QNone;
                  QVal x1153 -> (QVal ((mlt x1118) x1153));
                  QNode x1154 x1155 x1156 x1157 -> QError;
                };
              QNode x1124 x1125 x1126 x1127 ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> QNone;
                  QVal x1163 -> QError;
                  QNode x1164 x1165 x1166 x1167 ->
                    (QNode
                      (let
                        g=(\u->
                          (\v->
                            case  u  of {
                              QError  -> QError;
                              QNone  ->
                                case  v  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x3096 -> QNone;
                                  QNode x3097 x3098 x3099 x3100 -> QNone;
                                };
                              QVal x3081 ->
                                case  v  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x3101 -> (QVal ((mlt x3081) x3101));
                                  QNode x3102 x3103 x3104 x3105 -> QError;
                                };
                              QNode x3087 x3088 x3089 x3090 ->
                                case  v  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x3106 -> QError;
                                  QNode x3107 x3108 x3109 x3110 ->
                                    (QNode ((g x3087) x3107) ((g x3088) x3108) ((g x3089) x3109) ((g x3090) x3110));
                                };
                            }))
                      in
                        ((g x1124) x1164))
                      (let
                        h=(\w->
                          (\p->
                            case  w  of {
                              QError  -> QError;
                              QNone  ->
                                case  p  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x2536 -> QNone;
                                  QNode x2537 x2538 x2539 x2540 -> QNone;
                                };
                              QVal x2521 ->
                                case  p  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x2541 -> (QVal ((mlt x2521) x2541));
                                  QNode x2542 x2543 x2544 x2545 -> QError;
                                };
                              QNode x2527 x2528 x2529 x2530 ->
                                case  p  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x2546 -> QError;
                                  QNode x2547 x2548 x2549 x2550 ->
                                    (QNode ((h x2527) x2547) ((h x2528) x2548) ((h x2529) x2549) ((h x2530) x2550));
                                };
                            }))
                      in
                        ((h x1125) x1165))
                      (let
                        f1=(\r->
                          (\s->
                            case  r  of {
                              QError  -> QError;
                              QNone  ->
                                case  s  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x1976 -> QNone;
                                  QNode x1977 x1978 x1979 x1980 -> QNone;
                                };
                              QVal x1961 ->
                                case  s  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x1981 -> (QVal ((mlt x1961) x1981));
                                  QNode x1982 x1983 x1984 x1985 -> QError;
                                };
                              QNode x1967 x1968 x1969 x1970 ->
                                case  s  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x1986 -> QError;
                                  QNode x1987 x1988 x1989 x1990 ->
                                    (QNode ((f1 x1967) x1987) ((f1 x1968) x1988) ((f1 x1969) x1989) ((f1 x1970) x1990));
                                };
                            }))
                      in
                        ((f1 x1126) x1166))
                      (let
                        g1=(\t->
                          (\x1->
                            case  t  of {
                              QError  -> QError;
                              QNone  ->
                                case  x1  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x1416 -> QNone;
                                  QNode x1417 x1418 x1419 x1420 -> QNone;
                                };
                              QVal x1401 ->
                                case  x1  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x1421 -> (QVal ((mlt x1401) x1421));
                                  QNode x1422 x1423 x1424 x1425 -> QError;
                                };
                              QNode x1407 x1408 x1409 x1410 ->
                                case  x1  of {
                                  QError  -> QError;
                                  QNone  -> QNone;
                                  QVal x1426 -> QError;
                                  QNode x1427 x1428 x1429 x1430 ->
                                    (QNode ((g1 x1407) x1427) ((g1 x1408) x1428) ((g1 x1409) x1429) ((g1 x1410) x1430));
                                };
                            }))
                      in
                        ((g1 x1127) x1167)));
                };
            };
          QVal x95 ->
            case  y  of {
              QError  -> QError;
              QNone  ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> (QVal x95);
                  QVal x919 -> (QVal x95);
                  QNode x920 x921 x922 x923 -> (QVal x95);
                };
              QVal x894 ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> (QVal x95);
                  QVal x929 -> (QVal ((pls x95) ((mlt x894) x929)));
                  QNode x930 x931 x932 x933 -> QError;
                };
              QNode x900 x901 x902 x903 ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> (QVal x95);
                  QVal x939 -> QError;
                  QNode x940 x941 x942 x943 -> QError;
                };
            };
          QNode x101 x102 x103 x104 ->
            case  y  of {
              QError  -> QError;
              QNone  ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> (QNode x101 x102 x103 x104);
                  QVal x259 -> (QNode x101 x102 x103 x104);
                  QNode x260 x261 x262 x263 -> (QNode x101 x102 x103 x104);
                };
              QVal x234 ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> (QNode x101 x102 x103 x104);
                  QVal x269 -> QError;
                  QNode x270 x271 x272 x273 -> QError;
                };
              QNode x240 x241 x242 x243 ->
                case  z  of {
                  QError  -> QError;
                  QNone  -> (QNode x101 x102 x103 x104);
                  QVal x279 -> QError;
                  QNode x280 x281 x282 x283 ->
                    (QNode (((f x101) x240) x280) (((f x102) x241) x281) (((f x103) x242) x282) (((f x104) x243) x283));
                };
            };
        })))
   in
  (((f m1) m2) m3)