module Youtan.Regex.FMTestCases where

-- TODO: Test repeaters applied to repeaters.
--
--
cases :: [ ( String, String, Bool, String ) ]
cases =
  [ (             "",        "",  True, "Empty regex with empty string" )
  , (             "",     "abc", False, "Empty regex with none empty string" )
  , (            "a",       "a",  True, "Single char regex with same char string")
  , (            "a",       "b", False, "Single char regex with another char string")
  , (          "abc",     "abc",  True, "Plain regex with same plain string")
  , (          "abc",     "acc", False, "Plain regex with different plain string")
  , (          "a|b",       "a",  True, "Literal disjunction matching branch 1")
  , (          "a|b",       "b",  True, "Literal disjunction matching branch 1")
  , (          "a|b",       "c", False, "Literal disjunction matching nothing")
  , (    "a|b|c|d|e",       "e",  True, "Literal disjunction with few branches and matching")
  , (    "a|b|c|d|e",       "t", False, "Literal disjunction with few branches without matching")
  , (         "(a|)",        "",  True, "Empty disjunction matching")
  , (           "a*",        "",  True, "Kleene matches empty string")
  , (           "a*",       "a",  True, "Kleene matches single char string")
  , (           "a*",    "aaaa",  True, "Kleene matches few char string")
  , (           "a*",       "b", False, "Kleene fails for another char")
  , (         "\\d?",        "",  True, "Zero or one matches with empty string")
  , (         "\\d?",       "1",  True, "Zero or one matches exactly one char string")
  , (         "\\d?",      "23", False, "Zero or one fails for few chars string")
  , (         "\\d+",        "", False, "One or more fails with empty string")
  , (         "\\d+",       "1",  True, "One or more matches exactly one char string")
  , (         "\\d+",     "234",  True, "One or more matches for few chars string")
  , ( "(\\d\\d:?)+" ,      "23",  True, "Nested counter matches once")
  , ( "(\\d\\d:?)+" ,  "12:34:",  True, "Nested counter matches few times")
  , ( "(\\d\\d:?)+" ,    "12:3", False, "Nested counter fails on second time")
  ]
