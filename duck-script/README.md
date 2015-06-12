DuckScript
==============

A basic string-based script oriented on parsing raw language with SentenceID, dealing with a "context"...
Very suitable for making bots or text based adventures. "stringToDuckScript" reads a script, "runDuckScript" runs it,
resulting in IO String

Example code: (only some features shown)

```
If
  Block
    Parse
      $input
      SentenceID
        hi
        Delim
        Tagged
          after
          Anything
    If
      Block
        Parse
          $after
          SentenceID
            Null
        hey there! you just said "\""hi"\""
      Block
        True
        hey there! you said "\""$after"\"" after you said "\""hi"\""
  Block
    Parse
      $input
      SentenceID
        Anything
        quit
        Anything
    Random
      quitting...
      goodbye world!
      :'( goodbye
      cya round
  Block
    True
    Say what?
```