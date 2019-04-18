module WitnessTree where

import Parser
import Automata

import Control.Monad
import System.Process

import Data.List as L
import Data.Set as S
import Data.Map as M
import Data.Maybe


-- DEBUG
import System.IO.Unsafe
import Debug.Trace



data InputTree = Q State
               | T [(Message, InputTree)]
                 deriving (Show, Eq, Ord)

                    
data CTree = Node
             { label :: IValue
             , children :: [(TLabel, CTree)]
             }
           deriving (Show, Eq, Ord)
                    

type Value = (State, InputTree)
type IValue = (String, Value)
type TLabel = (Bool, Label)
type Edge = (IValue, (TLabel, IValue))
type NodeMap = Map IValue String
type Ancestors = Map IValue IValue


data EInputTree = XVar XVariable
                | E [(Message, EInputTree)]
                | I [EInputTree]
                deriving (Show, Eq, Ord)
                         
type XVariable = (State, IValue) 

type Envi= Map XVariable EInputTree



  
allNodes :: CTree -> [IValue]
allNodes (Node v xs) = (v:(concat $ L.map (allNodes . snd) xs))


checkEmbedding :: Bool -> Machine -> Ancestors -> CTree -> Bool
checkEmbedding nomin m2 ancs tree =
  let prodenvi = genProduceEnvi m2 ancs tree
      consenvi = genConsumeEnvi ancs tree
  in case do prodm <- eqsToAutomata nomin prodenvi (XVar $ mkProdRoot tree)
             consm <- eqsToAutomata nomin consenvi (XVar $ mkConsRoot tree)
             return $ embeddingAutomata prodm consm
     of
      Nothing -> trace ("Environment not defined!") False
      Just s -> s



getAllPaths :: CTree -> [([TLabel], IValue)]
getAllPaths (Node v []) = [([],v)]
getAllPaths (Node v ys) =  
  concat $ L.map (\x -> let ps = getAllPaths (snd x)
                        in L.map (\(p,n) -> ((fst x):p,n)) ps
                 ) ys             
        
genProduceEnvi :: Machine -> Ancestors -> CTree -> Envi
genProduceEnvi m ancs t = M.fromList $ (mkProdRoot t, mkEITRoot (label t) (snd $ snd $ label t)):(helper (states m) t) 
  where helper st (Node v []) = []
        helper st n@(Node v xs) = 
          let ret = concat $ (L.map (\q -> case eit q n of
                                          Nothing -> []
                                          Just tt -> [((q,v), tt)]
                                      ) 
                              st)
          in ret
             ++
             (concat $ L.map ((helper st) . snd) xs)

        eit q n = mkNEITree m ancs q n
          -- case sequence $ L.map (\(psi,ni) -> mkEITree m ancs q psi ni) $ getAllPaths n of
          -- Nothing -> Nothing 
          -- Just so -> Just $ I so

          
mkEITree :: Machine -> Ancestors -> State -> [TLabel] -> IValue -> Maybe EInputTree
mkEITree m ancs q path leaf =
  do at <- accTree m q (sndProj path)
     anc <- M.lookup leaf ancs
     return $ helper anc at
  where helper anc (Q q') = XVar (q', anc)
        helper anc (T xs) = E $ L.map (\(x,y) -> (x, helper anc y)) xs


tr :: State -> Ancestors -> CTree -> Maybe EInputTree
tr q ancs (Node n []) = (M.lookup n ancs) >>= (\x -> return $ XVar (q, x))
tr q ancs (Node n ys) = Just $ XVar (q, n)


addETree :: Map State EInputTree -> InputTree -> EInputTree
addETree mp (Q q) = case M.lookup q mp of
                     Nothing -> error$ "Not found: "++q
                     Just s -> s
addETree mp (T xs) = E $ L.map (\(a,t) -> (a, addETree mp t)) xs

mkNEITree :: Machine -> Ancestors -> State -> CTree -> Maybe EInputTree
mkNEITree m ancs q (Node v []) = Nothing -- boundary => no equation
mkNEITree m ancs q (Node v xs)
  | isSendTree xs = do at <- inTree m q
                       nmp <- sequence $
                              L.map (\(x,y) -> case y of
                                                Nothing -> Nothing
                                                Just s -> Just (x,s)) $
                              [(qi, msuccessor m qi la >>= (\x -> tr x ancs n')) | ((b,la), n') <- xs, qi <- leavesIT at]
                       return $ addETree (M.fromList nmp) at 

                    
  | otherwise = (sequence (L.map (\(l,n') -> tr q ancs n') xs)) >>= (\x -> return $ I x)


mkEITRoot :: IValue -> InputTree -> EInputTree
mkEITRoot v (Q q) = XVar (q, v)
mkEITRoot v (T xs) = E $ L.map (\(x,y) -> (x, mkEITRoot v y)) xs

isSendTree (((b,(Send,m)),t):xs) = True
isSendTree (((b,(Receive,m)),t):xs) = False                    
        
genConsumeEnvi :: Ancestors -> CTree -> Envi
genConsumeEnvi ancs t = M.fromList $ helper t
  where helper (Node v []) = []
        helper (Node v xs) 
          | isSendTree xs = [(("", v), I $ L.map (\(n,t) -> XVar ("", (trsucc t))) xs)]++(cont xs)
          | otherwise = [(("", v), E $ L.map (\(n,t) -> (snd . snd $ n, XVar ("", (trsucc t)))) xs)]++(cont xs)
        trsucc (Node v []) = case M.lookup v ancs of
                              Nothing -> error $ "Variable "++(show v)++" not found in ancestors:\n"++(show ancs)++"."
                              Just a -> a
        trsucc (Node v xs) = v
        cont xs = concat $ L.map (helper . snd) xs
            
            
checkingAlgorithm ::  Bool ->  Bool -> Int -> LocalType -> LocalType -> IO ()
checkingAlgorithm debug nomin bound t u =
  let m1 = type2Machine nomin "-" t
      m2 = type2Machine nomin "+" u
      res = buildTree m1 m2 [] "0" (tinit m1, Q $ tinit m2 )
  in do when debug $ do machine2file m1 "m1"
                        machine2file m2 "m2"
                        mkPicture "m1_cfsm.dot" "m1.png"
                        mkPicture "m2_cfsm.dot" "m2.png"
                        -- putStrLn $ show res
        case res of
         Nothing -> putStrLn "False"
         Just (t, mp) -> let ct = mkCandidateSubtrees mp t
                             sct = L.map (isSafeSubTree m1 m2 mp) ct
                             fi = L.map (isFinite m1 m2 mp) ct
                             produce = L.map (genProduceEnvi m2 mp) ct
                             consume = L.map (genConsumeEnvi mp)  ct
                             embs = L.map (checkEmbedding nomin m2 mp) ct
                         in do if and $ L.map (\(x,y,z) -> x || (y && z)) $ zip3 fi sct embs
                                 then putStrLn $ "True"
                                 else putStrLn $ "Maybe"
                               when debug $ do putStrLn $ "Finite subtrees: "++(show fi)
                                               putStrLn $ "Canditate subtrees: "++(show sct)
                                               putStrLn $ "Tree embedding: "++(show embs)
                                               --
                                               writeToFile "simulation_tree.dot" (printTrees mp [t])
                                               mkPicture "simulation_tree.dot" "simulation_tree.png"
                                               writeToFile "candidate_trees.dot" (printTrees mp ct)
                                               mkPicture "candidate_trees.dot" "candidate_trees.png"
                                               --
                                               genEIT nomin m2 mp ct

          
genEIT :: Bool -> Machine -> Ancestors -> [CTree] -> IO ()         
genEIT nomin m2 mp ts = helper 1 ts 
  where helper i [] = putStrLn $ (show $ i-1)++" producers/consummers printed."
        helper i (x:xs) =
          do let pt = genProduceEnvi m2 mp x
                 ct = genConsumeEnvi mp x
                 eqt = eqsToAutomata nomin pt (XVar $ mkProdRoot x)
                 eqc = eqsToAutomata nomin ct (XVar $ mkConsRoot x)
             if (isJust eqc) && (isJust eqt)
               then do machine2file (fromJust eqt) ("pt"++(show i))
                       machine2file (fromJust eqc) ("ct"++(show i))
                       mkPicture ("pt"++(show i)++"_cfsm.dot") ("pt"++(show i)++".png")
                       mkPicture ("ct"++(show i)++"_cfsm.dot") ("ct"++(show i)++".png")
                       helper (i+1) xs
               else helper (i+1) xs


mkProdRoot :: CTree -> (State, IValue)
mkProdRoot x = ("ROOT", label x)

mkConsRoot :: CTree -> (State, IValue)
mkConsRoot x = ("",label x)
  
frontIT :: InputTree -> Set Message
frontIT (T xs) = S.fromList $ L.map fst xs
frontIT _ = error $ "[WitnessTree] InputTree is empty! (front)"

nextIT :: InputTree -> Message -> InputTree
nextIT (T []) _ = error $ "[WitnessTree] InputTree has no successor! (next)"
nextIT (T xs) a = case L.filter (\(b,t) -> b==a) xs of
                   [] ->  error $ "[WitnessTree] InputTree has no successor for "++(show a)++" in "++(show xs)
                   (x:xs) -> snd x
nextIT _ _ = error $ "[WitnessTree] InputTree is empty! (next)"

leavesIT :: InputTree -> [State]
leavesIT t = L.map snd $ heightIT t 


minHeight :: InputTree -> Int
minHeight t =  minimum $ L.map fst $ heightIT t 

heightIT :: InputTree -> [(Int, State)]
heightIT t = nub $ helper 0 t
  where helper i (T xs) = concat $ L.map (helper (i+1) . snd) xs
        helper i (Q q) = [(i,q)]

hasAccum :: InputTree -> Bool
hasAccum (T xs) = True
hasAccum _ = False

uniqueState :: InputTree -> State
uniqueState (Q q) = q
uniqueState _ =  error $ "[WitnessTree] InputTree is not empty! (uniqueState)"

inTree :: Machine -> State -> Maybe InputTree
inTree m q
  | not (ampersand Receive m q) = Nothing
  | (inBarb m q) == S.empty = Just $ Q q
  | otherwise =
      let qs = L.map snd $ L.filter (\(x,(y,z)) -> x==q) $ transitions m
          nlist =  L.map (\(l,t) -> case inTree m t of
                                     Nothing -> Nothing
                                     Just nt -> Just (snd l, nt)
                         ) qs

      in case sequence nlist of
          Nothing -> Nothing
          Just ys -> Just $ T ys


isFinalConf :: Machine -> Machine -> Value -> Bool
isFinalConf m1 m2 (p, it) = (not $ hasAccum it) && (isFinal m1 p) && (isFinal m2 (uniqueState it))
          
oneStep :: Machine -> Machine -> Value -> Maybe [(TLabel, Value)]
oneStep m1 m2 (p, it)
  | isFinalConf m1 m2 (p,it) = Just []
      --
  | (not $ hasAccum it) && (isInput m1 p) && (isInput m2 (uniqueState it))
    && ((inBarb m2 (uniqueState it)) `isSubsetOf` (inBarb m1 p)) =
      let  psmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==p) $ transitions m1
           qsmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==(uniqueState it)) $ transitions m2               
           next = L.nub $ [(x,y,a) | (a,x) <- psmoves, (b,y) <- qsmoves, a==b]
           npairs = L.map (\(x,y,l) -> 
                            ((False, l), (x, Q y))
                          ) next
      in Just npairs
         --
  | (not $ hasAccum it) &&  (isOutput m1 p) && (isOutput m2 (uniqueState it))
    && ((outBarb m1 p) `isSubsetOf` (outBarb m2 (uniqueState it))) =
      let psmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==p) $ transitions m1              
          qsmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==(uniqueState it)) $ transitions m2
          next = L.nub $ [(x,y,a) | (a,x) <- psmoves, (b,y) <- qsmoves, a==b]
          npairs = L.map (\(x,y,l) -> 
                           ((True, l), (x, Q y))
                         ) next
      in Just npairs
         --
  | (hasAccum it) && (isInput m1 p) && ((frontIT it) `isSubsetOf`  (inBarb m1 p)) =
      let psmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==p) $ transitions m1
          npairs = L.map (\y -> 
                           ((True, (Receive, y)), (successor m1 p (Receive, y), nextIT it y))
                         ) (S.toList $ frontIT it)
      in Just npairs
         --
  | (isOutput m1 p) && (ampersand Send m1 p) = 
    let tmpmap = L.map (\qi -> case  inTree m2 qi of
                                             Nothing -> Nothing
                                             Just t -> Just (qi, t)
                       ) (leavesIT it)
    in case sequence tmpmap of
        Nothing -> Nothing
        Just ls -> let itmap = M.fromList ls
                       qjhs = concat $ L.map leavesIT $ M.elems itmap
                       psmoves = L.map snd $ L.filter (\(x,(y,z)) -> x==p) $ transitions m1
                       npairs =  L.map (\(y,z) -> 
                                         ((True, y), (z, succInTree m2 y $ addTree itmap it))
                                       ) psmoves
                  in if and $ L.map (\x -> (outBarb m1 p) `isSubsetOf` (outBarb m2 x)) qjhs
                     then Just npairs
                     else Nothing
  | otherwise = Nothing
                      
    
succInTree :: Machine -> Label -> InputTree -> InputTree
succInTree m l (Q q) = Q $ successor m q l
succInTree m l (T xs) = T $ L.map (\(a,t) -> (a, succInTree m l t)) xs
  
addTree :: Map State InputTree -> InputTree -> InputTree
addTree mp (Q q) = mp!q
addTree mp (T xs) = T $ L.map (\(a,t) -> (a, addTree mp t)) xs



buildTree :: Machine -> Machine -> [(IValue, TLabel)] -> String -> Value -> Maybe (CTree, Map IValue IValue)
buildTree m1 m2 ps i val
  | val `L.elem` (L.map (snd . fst) ps) =
      let anc = fst . head $ L.filter (\((j,x),y) -> x == val) ps
      in Just (Node (i, val) [], M.singleton (i, val) anc)
  | otherwise =
  case getAncestorPair (L.map fst ps) (i,val) of
   Nothing -> cont
   Just (ni,nj) -> if extract (getIntermPath ps ni nj) (snd . snd $ ni) (snd val)
                   then Just (Node (i, val) [], M.singleton (i, val) ni)
                   else cont
  where cont = case oneStep m1 m2 val of
          Nothing -> Nothing
          Just next -> let rets = snd $
                                  mapAccumL
                                  (\j (x,y) -> (j+1, (x, buildTree m1 m2 (ps++[((i, val),x)]) (i++(show $ j+1)) y))) 0 next
                           chs = L.map (\(x,t) -> case t of
                                         Nothing -> Nothing
                                         Just (t', m') -> Just ((x,t'), m')) rets
                       in case sequence chs of
                           Nothing -> Nothing
                           Just sqs -> Just (Node (i, val) (L.map fst sqs), M.unions $ L.map snd sqs)
                    




extract :: [Message] -> InputTree -> InputTree -> Bool
extract w (Q _) (Q _) = True
extract [] ai ak = (minHeight ai) <= (minHeight ak)
extract w (Q _) _ = False
extract w _ (Q _) = True
extract (a:as) (T xs) (T ys)
  | (a `S.member` (frontIT (T xs))) && (a `S.member` (frontIT (T ys))) =
      extract as (nextIT (T xs) a) (nextIT (T ys) a)
  | otherwise = False

sameTop :: InputTree -> InputTree -> Bool
sameTop (Q q) (Q q') = q==q'
sameTop (T xs) (T ys) = (L.map fst xs) == (L.map fst ys)
sameTop _ _ = False

-- returns a pair (ni, nj) if there's one
getAncestorPair :: [IValue] -> IValue -> Maybe (IValue, IValue)
getAncestorPair xs v =
  let removeEmpty [] acc = acc
      removeEmpty ((i,(p,it)):ps) acc
        | hasAccum it = removeEmpty ps (acc++[(i,(p,it))])
        | otherwise = removeEmpty ps []
      simVal (i,(p1, t1)) (j,(p2, t2)) =
        (p1==p2)
        &&
        (
          (t1==t2)
          ||
          (
            -- (sameTop t1 t2)     
            -- &&
            ((S.fromList $ leavesIT t1) `isSubsetOf` (S.fromList $ leavesIT t2))
          )
        )
      hasAncestor xs v = L.filter (\nn -> simVal nn v) (removeEmpty xs [])

  in case hasAncestor xs v of
      [] -> Nothing
      ys -> case hasAncestor (tail ys) (head ys) of
             [] -> Nothing
             zs -> Just (head ys, last zs)
 

getIntermPath :: [(IValue, TLabel)] -> IValue -> IValue -> [Message]
getIntermPath [] ni nj = []
getIntermPath ((v,l):xs) ni nj
  | (v==ni) = (proj l)++(helper xs nj)
  | otherwise = getIntermPath xs ni nj
  where proj (b, (Send, l)) = []
        proj (b, (Receive, l)) = [l]
        helper [] n = []
        helper ((v,k):ys) n
          | (v==n) = []
          | otherwise = helper ys n

mkPicture :: FilePath -> FilePath -> IO ()
mkPicture file output =
  let cmd = "dot -Tpng "++file++" -o "++output
  in do out <- readProcess "bash" ["-c", cmd] []
        return ()


mkCandidateSubtrees :: Map IValue IValue -> CTree -> [CTree]
mkCandidateSubtrees mp t = helper realancs t
  where helper ancs (Node v xs)
          | v `L.elem` ancs = [Node v xs]
          | otherwise = concat $ L.map (\(x,c) -> helper ancs c) xs
        realancs = L.map snd $ L.filter (\((i,k),(j,v)) -> v /= k) $ M.toList mp
        
-- -------------------- Finite Subtree --------------------------

isFinite :: Machine -> Machine -> Ancestors -> CTree -> Bool
isFinite m1 m2 mp t = helper [] t
  where helper _ (Node (i,v) []) =
          (isFinalConf m1 m2 v)
          ||
          case M.lookup (i,v) mp of
           Nothing -> False
           Just (j,n) -> n==v 
        helper seen (Node (i,v) xs)
          | v `L.elem` seen = True
          | otherwise = and $ L.map ((helper (v:seen)) . snd) xs


-- -------------------- Safe Subtree --------------------------


isSafeSubTree :: Machine -> Machine -> Ancestors -> CTree -> Bool
isSafeSubTree m1 m2 ancs t = -- (noBadPair m1 m2 t)
                             -- &&
                             (goodAccumulation m1 m2 ancs t)

noBadPair :: Machine -> Machine -> CTree -> Bool
noBadPair m1 m2 (Node (i, (p,it)) xs) = case it of
  (Q q) -> (isOutput m1 p) && (and $ L.map ((noBadPair m1 m2) . snd) xs)
  (T _) -> (and $ L.map ((noBadPair m1 m2) . snd) xs)


rcvProj :: [TLabel] -> [Message]
rcvProj xs = L.map snd $ L.filter (\(x,y) -> x==Receive) $ L.map snd xs 

sndProj :: [TLabel] -> [Message]
sndProj xs = L.map snd $ L.filter (\(x,y) -> x==Send) $ L.map snd xs

goodAccumulation :: Machine -> Machine -> Ancestors -> CTree -> Bool
goodAccumulation m1 m2 mpans t =
  (allPathReceive mpans t)
  &&
  (strictAccumulation m2 mpans t)


allPathReceive :: Ancestors -> CTree -> Bool
allPathReceive ancs t = helper (M.elems ancs) t
  where helper as (Node v@(i,n) xs)
          | v `L.elem` as = and $ L.map (findReceive . snd) xs
          | otherwise = and $ L.map ((helper as) . snd) xs
        isRcv (b,(Receive, _)) = True
        isRcv (b,(Send, _)) = False                                         
        findReceive (Node v xs) = and $ L.map (\(x,y) -> (isRcv x) || findReceive y) xs

strictAccumulation :: Machine -> Ancestors -> CTree -> Bool 
strictAccumulation m mp t = helper (M.elems mp) t
  where helper ancs t@(Node n@(i,(p,it)) xs)
          | n `L.elem` ancs =
              let quants = [(qi, path, n', it) | (path, n') <- getAllPaths t, qi <- leavesIT it]
              in and $ L.map 
                 (\(qi,psi,n'@(i',(p',it')),it) -> 
                   (
                     case leavesIT <$> accTree m qi (sndProj psi) of
                      Nothing -> False
                      Just ss -> (S.fromList ss)
                                 `isSubsetOf` 
                                 (S.fromList $ leavesIT it')
                   )
                   &&
                   (
                     case closest m (minHeight it) qi (L.map snd psi) of
                      Nothing -> False
                      Just h -> h >= (minHeight it)
                   )
                 )
                 quants
         
          | otherwise = and $ L.map ((helper ancs) . snd) xs
                        -- 
                       
closest :: Machine -> Int -> State -> [Label] -> Maybe Int
closest m n q [] = Just n
closest m n q ((Receive, a):xs) = closest m (n-1) q xs
closest m n q ((Send, a):xs) =
  (heightIT <$> accTree m q [a])
  >>=
  ( \ys ->
     minimum
     $
     L.map (\(i,qi) -> closest m (n+i) qi xs) ys
  )
  
accTree :: Machine -> State -> [Message] -> Maybe InputTree
accTree m q [] = Just (Q q)
accTree m q (y:ys) =
  do acc <- inTree m q
     nn <- sequence $ L.map (\qi -> do qi' <- msuccessor m qi (Send, y)
                                       tr <- accTree m qi' ys
                                       return $ (qi, tr)
                            ) $ leavesIT acc
     return $ addTree (M.fromList nn) acc


     -- ----------------- TREE EMBEDDING / COMPATIBILITY ----------------
                           


eqsToAutomata :: Bool -> Envi -> EInputTree -> Maybe Machine
eqsToAutomata nomin envi t = 
  do ti <- mkTrans "init" t
     tt <- concat <$>
           (sequence $ L.map (\(k,v) -> mkTrans (printVar k) v) $ M.toList envi)
     let rs = reachableStates (ti++tt) [] ["init"]
         ct = L.filter (\(s,(l,t)) -> (t `L.elem` rs) && (s `L.elem` rs)) (tt++ti)
         m = Machine { states = nub rs
                     , tinit = "init"
                     , transitions = nub ct
                     , accepts = []
                     }
     return $ m
       -- if not nomin
       --        then m
       --        else minimizeHop $ rename (naming m) m
        
  where printVar (q,(i,v)) = q++"X"++i
        mkNext i v = i++v
        comb a (Just as) = Just (a:as)
        comb a (Nothing) = Nothing
        mkSilentTrans src i (XVar v) = Just [(src, ((Send, i), printVar v))]
        mkSilentTrans src i t =  
          let nt = src++"S"++i
          in comb (src, ((Send, i), nt)) (mkTrans nt t)   
             
        mkReceiveTrans src a (XVar v) = Just [(src, ((Receive, a), printVar v))]
        mkReceiveTrans src a t = 
          let nt = src++"R"++a
          in comb (src, ((Receive, a), nt)) (mkTrans nt t)

        mkTrans :: String -> EInputTree -> Maybe [Transition]
        mkTrans src (XVar v) =
          case M.lookup v envi of
          Nothing -> trace ("Variable "++(show v)++" not found.") -- in\n"++(show envi)++".")
                     Nothing
          Just vv -> Just [(src, ((Send, "_tau_"), printVar v))] 
        mkTrans src (E xs) =
          concat <$> 
          (sequence $ L.map (\(ai,ti) -> mkReceiveTrans src ai ti) xs)
        mkTrans src (I xs) =
          concat <$>
          (sequence $
           snd $ L.mapAccumL (\i ti -> (i+1, mkSilentTrans src (show i) ti)) 0 xs)
        reachableStates trans seen [] = seen
        reachableStates trans seen (src:xs)
          | src `L.elem` seen = reachableStates trans seen xs
          | otherwise =
              let succs = L.map snd $ L.filter (\(s,(lab,t)) -> s==src) trans
              in reachableStates trans (src:seen) $ xs++(L.map snd succs)
         

embeddingAutomata :: Machine -> Machine -> Bool
embeddingAutomata prod cons = helper [] [(tinit prod, tinit cons)]
  where helper seen [] = True
        helper seen ((p,c):zs)
          | (p,c) `L.elem` seen = helper seen zs
                                  
          | (isOutput prod p) =
              let next = L.map (\(x,y) -> (y, c)) (successors prod p)
              in helper ((p,c):seen) (zs++next)
                 
          | (isOutput cons c) =
              let next = L.map (\(x,y) -> (p, y)) (successors cons c)
              in helper ((p,c):seen) (zs++next)
                 
          | (isInput prod p) && (isInput cons c) =
              let psmoves = successors prod p
                  csmoves = successors cons c
                  next = L.map
                         (\(x,y) -> (y, successor cons c x))
                         psmoves
              in ((S.fromList $ L.map fst psmoves) `isSubsetOf` (S.fromList $ L.map fst csmoves))
                 &&
                 (helper ((p,c):seen) (zs++next))

          | otherwise = False -- error $ "Embedding relation does not hold for: "++(show (p,c))


-- --------------------- PRINTING STUFF -----------------

printNodeLabel :: IValue -> String
printNodeLabel (i,(p,it)) = "<<table color='blue'><tr><td><b>"++p++"</b></td><td>"
                            ++(inputTreeToHTLM it)
                            ++"</td></tr></table>>"

mkNodeMap :: Int -> [IValue] -> NodeMap
mkNodeMap i xs = M.fromList $ L.map (\(id,y) -> ((id,y),"node"++(show i)++id)) xs

printNodeId :: NodeMap -> IValue -> String
printNodeId  map v = case M.lookup v map of
                      Just s -> s
                      Nothing -> error $ "Node "++(show v)++" not found."

printNode :: NodeMap -> IValue -> String
printNode map v = (printNodeId map v)++"[label="++(printNodeLabel v)++"];"

mkGraph :: CTree -> (IValue, [Edge])
mkGraph (Node v xs) = (v, helper v xs)
  where helper n ys = concat $
                          L.map (\(l, (Node w zs)) -> (n,(l, w)):(helper w zs)) ys


printEdge :: NodeMap -> Edge -> String
printEdge map (s,(l,t)) = (printNodeId map s)++" -> "++(printNodeId map t)++"[label=\""++(printEdgeLabel l)++"\"];"

printAncestorEdge :: NodeMap -> (IValue, IValue) -> String
printAncestorEdge map (s,t) = (printNodeId map s)++" -> "++(printNodeId map t)++"[style=dashed];"

printGraph :: Int -> IValue -> [Edge] -> Ancestors -> String
printGraph i init edges ancestors =
  let nodes = nub $ concat $ L.map (\(s,(l,t)) -> [s,t]) edges
      nmap = mkNodeMap i nodes
      snodes = intercalate "\n" $ L.map (printNode nmap) nodes
      sedges = intercalate "\n" $ L.map (printEdge nmap) edges
      ancs =  intercalate "\n" $ L.map (printAncestorEdge nmap) $
              L.filter (\(t,s) -> (t `L.elem` nodes) && (s `L.elem` nodes)) $
              M.toList ancestors
  in snodes++"\n"
     ++sedges++"\n"
     ++ancs++"\n"

printTree :: Int -> Ancestors -> CTree -> String
printTree i ancs t = let (init, edges) = mkGraph t
                     in printGraph i init edges ancs

printTrees :: Ancestors -> [CTree] -> String
printTrees ancs list =  "digraph CTs { \n"++(helper $ snd $ mapAccumL (\x y -> (x+1, (x,y))) 0 list)++"}\n"
  where helper ((i,x):xs) = 
          let header = "subgraph cluster_"++(show i)
                       ++" {\n"
                       ++ "label = \""++(show i)++"\";\n"      
              footer = "}\n"
              tr = printTree i ancs x
          in (header++tr++footer)++"\n"++(helper xs)
        helper [] = ""
              
                           

printEdgeLabel :: TLabel -> String
printEdgeLabel (True, (Send, msg)) = "!"++msg
printEdgeLabel (True, (Receive, msg)) = "?"++msg
printEdgeLabel (False, (_, msg)) = "["++msg++"]"




inputTreeToHTLM :: InputTree -> String
inputTreeToHTLM (Q q) = "<b><font color='red'>"++q++"</font></b>"
inputTreeToHTLM (T xs) = inputForestToHTML xs

inputForestToHTML :: [(Message, InputTree)] -> String
inputForestToHTML [] = ""
inputForestToHTML [x] = pairToHTML x
inputForestToHTML xs = "<table color='black' border='1'><tr><td>"++(intercalate "</td><td>" $ L.map pairToHTML xs)++"</td></tr></table>"

pairToHTML :: (Message, InputTree) -> String
pairToHTML (m, t) = "<table color='black' border='0'><tr><td>"++(m)++"</td></tr>"
                    ++"<tr><td>"++(inputTreeToHTLM t)++"</td></tr>"
                    ++"</table>"

