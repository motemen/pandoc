module Tests.Readers.Hatena (tests) where

import Test.Tasty
import Tests.Helpers
import Text.Pandoc
import Text.Pandoc.Arbitrary ()
import Text.Pandoc.Builder

hatena :: String -> Pandoc
hatena = purely $ readHatena def{ readerStandalone = True }

infix 4 =:
(=:) :: ToString c
     => String -> (String, c) -> TestTree
(=:) = test hatena

tests :: [TestTree]
tests = [ testGroup "inline"
          [ "http bracket" =:
            "foo [http://www.example.com/] bar\n"
            =?>
            para (str "foo " <> (link "http://www.example.com/" "" $ str "http://www.example.com/") <> str " bar")
          , "http autolink" =:
            "foo http://www.example.com/ bar\n"
            =?>
            para (str "foo " <> (link "http://www.example.com/" "" $ str "http://www.example.com/") <> str " bar")
          , "http bracket with title" =:
            "foo [http://www.example.com/:title=example] bar\n"
            =?>
            para (str "foo " <> (link "http://www.example.com/" "" $ str "example") <> str " bar")
          , "inline tag" =:
            "foo <code>bar baz</code> blah\n"
            =?>
            para (str "foo " <> (code "bar baz") <> str " blah")
          ]
        , testGroup "para"
          [ "simple" =:
            unlines [ "foo"
                    , "bar"
                    ]
            =?>
            para (str "foo") <> para (str "bar")
          ]
        , testGroup "list"
          [ "bullet" =:
            unlines [ "- foo"
                    , "- bar"
                    , "- baz"
                    ]
            =?>
            bulletList [ plain (str "foo")
                       , plain (str "bar")
                       , plain (str "baz")
                       ]
          , "ordered" =:
            unlines [ "+ foo"
                    , "+ bar"
                    , "+ baz"
                    ]
            =?>
            orderedList [ plain (str "foo")
                        , plain (str "bar")
                        , plain (str "baz")
                        ]
          , "mixed" =:
            unlines [ "- foo"
                    , "+ bar"
                    , "- baz"
                    ]
            =?>
            bulletList [ plain (str "foo")
                       , plain (str "bar")
                       , plain (str "baz")
                       ]
          , "nested" =:
            unlines [ "- foo"
                    , "- bar"
                    , "-- baz"
                    , "--- blah"
                    ]
            =?>
            bulletList [ plain (str "foo")
                       , plain (str "bar")
                         <> bulletList
                            [ plain (str "baz")
                              <> bulletList [ plain (str "blah") ]
                            ]
                       ]
          , "mixed nested" =:
            unlines [ "- foo"
                    , "- bar"
                    , "-+ baz"
                    , "-+- blah"
                    ]
            =?>
            bulletList [ plain (str "foo")
                       , plain (str "bar")
                         <> orderedList
                            [ plain (str "baz")
                              <> bulletList [ plain (str "blah") ]
                            ]
                       ]
          ]
        , testGroup "blockquote"
          [ "simple" =:
            unlines [ ">>"
                    , "Hello, world"
                    , "<<"
                    ]
            =?>
            blockQuote (para (str "Hello, world"))
          , "prepost" =:
            unlines [ "pre"
                    , ">>"
                    , "Hello, world"
                    , "<<"
                    , "post"
                    ]
            =?>
            para (str "pre")
            <> blockQuote (para (str "Hello, world"))
            <> para (str "post")
          , "nested" =:
            unlines [ ">>"
                    , "outside"
                    , ">>"
                    , "inside"
                    , "<<"
                    , "<<"
                    ]
            =?>
            blockQuote (para (str "outside")
                       <> blockQuote (para $ str "inside")
                       )
          ]
        , testGroup "codeblock"
          [ "simple" =:
            unlines [ "pre"
                    , ">||"
                    , "Hello,"
                    , " world"
                    , "||"
                    , "||<"
                    , "post"
                    ]
            =?>
            para (str "pre")
            <> codeBlock "Hello,\n world\n||\n"
            <> para (str "post")
          , "with lang" =:
            unlines [ "pre"
                    , ">|haskell|"
                    , "-- Hello,"
                    , "--  world"
                    , "||<"
                    , "post"
                    ]
            =?>
            para (str "pre")
            <> codeBlockWith ("", ["haskell"], []) "-- Hello,\n--  world\n"
            <> para (str "post")
          ]
        , testGroup "combined"
          [ "bq and bl" =:
            unlines [ ">>"
                    , "foo"
                    , "- bar"
                    , "- baz"
                    , "<<"
                    , ""
                    , "- a [http://www.example.com/:title=example]"
                    , "-- b <code>foo</code> c"
                    , "--- d"
                    ]
            =?>
            blockQuote (para (str "foo")
                       <> bulletList [ plain (str "bar"), plain (str "baz") ]
                       )
            <> bulletList
               [ plain (str "a " <> (link "http://www.example.com/" "" $ str "example"))
                 <> bulletList
                    [ plain (str "b " <> code "foo" <> str " c")
                      <> bulletList
                         [ plain (str "d")
                         ]
                    ]
               ]
          ]
        , testGroup "table"
          [ "simple" =:
            unlines [ "|*foo|*bar|"
                    , "|foo1|bar1|"
                    , "|foo2|bar2|"
                    ]
            =?>
            simpleTable [ plain (str "foo"), plain (str "bar") ]
                        [ [ plain (str "foo1"), plain (str "bar1") ]
                        , [ plain (str "foo2"), plain (str "bar2") ]
                        ]
          ]
      ]
