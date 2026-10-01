-- Parameterized reference port of the repository workload; see IMPLEMENTATIONS.md.
{-# LANGUAGE BangPatterns #-}
import Data.Char (isSpace,ord)
import Data.List (isPrefixOf,foldl',intercalate)
import qualified Data.Map.Strict as M
import System.Environment
import Control.Monad

data Value = Text String | Number Int | Boolean Bool | Object (M.Map String Value) | Array [Value]
data Node = Literal String | Print String | If Bool String [Node] [Node] | For String String [Node] | With String String [Node] | Call String String
data Token = Raw String | Field String | Control String deriving Show
trim=reverse . dropWhile isSpace . reverse . dropWhile isSpace
trimEnd=reverse . dropWhile isSpace . reverse
splitOn c s=case break (==c) s of (a,[]) -> [a];(a,_:b) -> a:splitOn c b
untilEnd end = go []
  where go acc []=error ("unclosed token "++end)
        go acc s | end `isPrefixOf` s=(reverse acc,drop (length end) s)
        go acc (c:cs)=go (c:acc) cs
-- Scanner owns whitespace trimming; recursive descent creates nested AST blocks.
scan source = go source "" False
  where
    go [] buf _=if null buf then [] else [Raw (reverse buf)]
    go s buf trimNext
      | "{#" `isPrefixOf` s=let (_,rest)=untilEnd "#}" (drop 2 s) in go rest buf trimNext
      | "{{" `isPrefixOf` s=
          let (body,rest)=untilEnd "}}" (drop 2 s)
              left=not (null body) && body!!0=='-'
              right=not (null body) && last body=='-'
              literal=if left then trimEnd (reverse buf) else reverse buf
              command=trim (dropWhile (=='-') (reverse (dropWhile (=='-') (reverse body))))
          in (if null literal then [] else [Raw literal])++Control command:go rest "" right
      | "{" `isPrefixOf` s=let (body,rest)=untilEnd "}" (drop 1 s) in (if null buf then [] else [Raw (reverse buf)])++Field (trim body):go rest "" False
      | otherwise=case s of
          c:cs | trimNext && isSpace c -> go cs buf True
               | otherwise -> go cs (c:buf) False
parse source = fst (block [] (scan source))
  where
    block stops [] | null stops=([],[])
                   | otherwise=error "unclosed directive"
    block stops ts@(Control c:rest) | c `elem` stops=([],ts)
    block stops (token:rest)=
      let (node,tail)=case token of
            Raw s -> (Literal s,rest)
            Field s -> (Print s,rest)
            Control c -> case words c of
              "if":args ->
                let (body,end)=block ["else","endif"] rest
                    (other,tail)=case end of
                      Control "else":more -> let (other,done)=block ["endif"] more in (other,drop 1 done)
                      Control "endif":more -> ([],more)
                      _ -> error "unclosed if"
                in (If (args!!0=="not") (last args) body other,tail)
              ["for",alias,"in",path] -> let (body,end)=block ["endfor"] rest in (For alias path body,drop 1 end)
              ["with",path,"as",alias] -> let (body,end)=block ["endwith"] rest in (With path alias body,drop 1 end)
              ["call",name,"with",path] -> (Call name path,rest)
              _ -> error ("unknown directive "++c)
          (more,end)=block stops tail
      in (node:more,end)
lookupValue path root scope = foldl' field initial tail
  where
    head:tail=splitOn '.' path
    initial=if head=="@root" then root else case M.lookup head scope of Just v -> v;Nothing -> field root head
    field (Object fields) name=case M.lookup name fields of Just v -> v;Nothing -> error ("missing path "++path)
    field _ _=error "field on scalar"
truth (Boolean b)=b
truth (Text s)=not (null s)
truth (Array xs)=not (null xs)
truth (Number n)=n/=0
truth (Object xs)=not (M.null xs)
string (Text s)=s
string (Number n)=show n
string (Boolean b)=if b then "true" else "false"
string _=error "cannot format container"
escape=concatMap (\c -> case c of '&' -> "&amp;";'<' -> "&lt;";'>' -> "&gt;";'\"' -> "&quot;";'\'' -> "&#39;";_ -> [c])
type Engine = M.Map String [Node]
type Formatters = M.Map String (Value -> String)
render :: Engine -> Formatters -> String -> Value -> String
render engine formatters name root = nodes (engine M.! name) M.empty
  where
    value path scope=lookupValue path root scope
    nodes ast scope=concatMap (node scope) ast
    node scope (Literal s)=s
    node scope (Print path)=case map trim (splitOn '|' path) of
      [p] -> escape (string (value p scope))
      [p,f] -> (formatters M.! f) (value p scope)
      _ -> error "invalid formatter"
    node scope (If neg path body other)=nodes (if truth (value path scope)/=neg then body else other) scope
    node scope (For alias path body)=case value path scope of
      Array xs -> concat [nodes body (M.union (M.fromList [(alias,v),("@index",Number i),("@first",Boolean (i==0)),("@last",Boolean (i==length xs-1))]) scope) | (i,v)<-zip [0..] xs]
      _ -> error "for requires array"
    node scope (With path alias body)=nodes body (M.insert alias (value path scope) scope)
    node scope (Call name path)=render engine formatters name (value path scope)
object=Object . M.fromList
report :: Int -> Value
report n=object [("title",Text "Inventory <nightly>"),("empty",Boolean (n==0)),("rows",Array (map row [0..n-1])),("footer",Text "Generated & checked")]
  where row i=object [("name",Text ("Item <"++show i++">")),("featured",Boolean (i `mod` 3==0)),("details",object [("category",Text (if even i then "hardware" else "software")),("price",Number ((i+1)*7))]),("tags",Array (map Text ["stable","batch-"++show (i `mod` 4),"ready & tested"])),("raw_html",Text ("<span>SKU-"++replicate (max 0 (3-length (show i))) '0'++show i++"</span>"))]
checksum :: String -> Int
checksum=foldl' (\s c -> (s*31+ord c) `mod` 1000000007) 0
page="{# TinyTemplate application benchmark #}<main>\n<h1>{ title }</h1>\n{{ if not empty }}<section>{{ for row in rows -}}\n{{ call row with row }}\n{{- endfor }}</section>{{ else }}<p>No inventory.</p>{{ endif }}\n{{ call footer with footer }}\n</main>"
rowTemplate="<article class=\"{{ if featured }}featured{{ else }}standard{{ endif }}\">\n<h2>{ name }</h2>\n{{ with details as detail }}<p>{ detail.category }: { detail.price | currency }</p>{{ endwith }}\n<ul>{{ for tag in tags }}<li data-first=\"{ @first }\" data-last=\"{ @last }\">{ @index }:{ tag }</li>{{ endfor }}</ul>\n<div>{ raw_html | unescaped }</div>\n</article>"
main = do
  args <- getArgs
  let engine=M.fromList [("page",parse page),("row",parse rowTemplate),("footer",parse "<footer>{ @root }</footer>")]
      formatters=M.fromList [("unescaped",string),("currency",\v -> "$"++string v++".00")]
      dataValue=report (read (args!!0))
  values <- replicateM (read (args!!1)) $ do
    current <- getArgs
    let name=if read (current!!0)>=0 then "page" else error "negative rows"
        !n=checksum (render engine formatters name dataValue)
    pure n
  print (foldl' (\s n -> (s+n) `mod` 1000000007) 0 values)
