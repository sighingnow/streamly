# Streams are monadic lists

Streamly streams are a generalization of standard Haskell lists with two
crucial additions:

* A list is a _pure sequence_ of values whereas a stream is a _monadic
  sequence_ of values. A monadic sequence is created by executing a sequence of
  monadic actions.
* Streams support concurrent execution of monadic actions to produce the
  sequence. Concurrency is declarative and can be switched on or off using an
  appropriate combinator.

Lists and streams behave in the same way and have almost identical
APIs.  This is how you would create a pure sequence or a list:

```
> "hello" : "world" : []
["hello","world"]
```

The same way we can construct a monadic sequence or a stream:

```
import Streamly
import Streamly.Prelude ((|:))
import qualified Streamly.Prelude as S

> S.toList $ getLine |: getLine |: S.nil
hello
world
["hello","world"]
```

`nil` corresponds to `[]`, representing an empty stream. `|:` corresponds to
the `:` operator, adding another monadic action at the head of a stream.
`toList` converts the stream to a list. The monadic action `getLine` is
executed twice in a sequence, we enter "hello" and "world" on the terminal and
a stream of the entered strings is produced.

## Streams use the familiar list APIs

Streamly supports all the standadrd list APIs.  Stream APIs are almost
identical to the list APIs.  If you know how to use lists then you know how to
use streams.

For example, consider the following code snippet that uses standard lists:

```haskell
import Control.Concurrent

func = mapM (\x -> threadDelay 1000000 >> print x) $ replicate 10 1
main = func
```

Just replace `mapM` and `replicate` with the corresponding stream combinators
to get the stream version:

```haskell
import Control.Concurrent
import Streamly
import qualified Streamly.Prelude as S

func =   S.mapM (\x -> threadDelay 1000000 >> print x)
       $ S.replicateM 10 (return 1)
main = runStream func
```

## Streams are concurrent

Streams support concurrent generation, mapping and zipping by just using an
appropriate modifying combinator, without any other change to the code.
Therefore we can call them concurrent monadic lists.

Both, the list code and the stream code in the previous section print one
element every second.  To map the monadic delay action concurrently on all the
stream elements, just run the same code with `asyncly` combinator:

```haskell
main = runStream $ asyncly func
```

This prints all the 10 elements after one second because all the delay actions
run concurrently.

## Streams are monadic

Streams being monadic sequences, they additionally provide monadic versions of
the pure APIs.  Monadic APIs in streamly are generally suffixed with the letter
`M`.  For example, the monadic version of `replicate` is `replicateM` which
performs a monadic action `n` times:

```
replicateM :: (IsStream t, MonadAsync m) => Int -> m a -> t m a
```

We can use `replicateM` to generate a stream by performing an action multiple
times.  Additonally, it can be used to perform the action concurrently:

```haskell
import Control.Concurrent
import Streamly
import qualified Streamly.Prelude as S

main = runStream $ asyncly $
  S.map print $ S.replicateM 10 (threadDelay 1000000 >> return 1)
```

It prints `1` 10 times after a second. In real applications this could be a
request to a web server that you may want to perform multiple times
concurrently.

## Streams obviate the need for difference lists

Lists are notorious for append performance. Difference lists have been invented
to avoid that problem. Streamly streams do not suffer from that problem they
provide an O(1) append performance.

## Streams perform better than lists

Here is a performance comparison of pure lists and streamly streams:


## Use streams instead of lists

Pure lists are a special case of monadic streams and streams perform equal or
better than lists. Therefore, you can fearlessly use streams where you need
lists.  By using monadic streams instead of lists you get three crucial
benefits:

* You can use monadic actions anywhere in your code.  When using pure lists you
  may often realize that you have to print a warning or an error message
  somewhere in the middle of processing, how do you do that in pure code?  With
  monadic streams it is not a problem, they are already monadic, you can use IO
  or any other monadic code anywhere.
* You get the option to make your code concurrent for free.
* Your code will never suffer from append performance issues.

Pure lists can be treated as a special case of monadic streams using the
_Identity_ monad.

## Differences from lists

We strive to keep all stream APIs on the lines of `Data.List` module in the
`base` package as far as possible.  Minor differences in API signatures arise
due to streams being monadic.  Some of the differences are:

* Since `streamly` streams are monadic containers they are not instances of the
  standard `Foldable` and `Traversable` typeclasses meant to be used for pure
  containers. However, equivalent functionality is provided without using the
  type classes.
* Fold functions have slightly different signatures because they fold monadic
  containers instead of pure lists.
* Monadic traversal functions like `mapM` and `sequence` have slightly
  different signatures to accomodate monadic containers.

Another key difference arises because streamly avoids partial functions.
Partial functions like `tail`, `head` from lists have safe versions in streamly
returning a `Maybe`.
