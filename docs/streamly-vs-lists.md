# Streams are concurrent monadic lists

Streamly streams are a generalization of standard Haskell lists with two
crucial additions:

* A list is a _pure sequence_ of values whereas a stream is a _monadic
  sequence_ of values. A monadic sequence is created by executing a sequence of
  monadic actions.
* The monadic actions producing the sequence can be executed concurrently.
  Concurrency is declarative and can be switched on or off using an appropriate
  combinator.

## Pure Streams vs Monadic Streams

A list is a sequence of pure values or pure steps that produce a value on
evaluation.  Ideally we want to process each element in a sequence through a
pipeline of processing stages without first producing or accumulating all the
elements. We call this style of processing streaming and it runs in constant
memory because we do not accumulate data in memory, data gets consumed as soon
as it is produced.  If all the processing stages are pure, list can be used in
a streaming style processing. For example, this is streaming:

```
  replicate 10 1
    & zipWith (+) [1..10]
    & map (+1)
    & filter odd
    & sum
```

However, any operation that produces a list in a monadic context
cannot be interleaved with monadic computations without breaking streaming. At
that point we just have to accumulate the whole container before proceeding.
For example, this accumulates `xs` before it processes it:

```
  xs <- replicateM 10 (return 1)
  zipWith (+) [1..10] xs
    & map (+1)
    & filter odd
    & sum
```

To be able to consume elements from `replicateM` as it produces them we need a
monadic representation of the stream and monadic processing operations that can
consume the monadic stream lazily one element at a time. Streamly provides a
monadic representation of stream, it is nothing but a monadic version of lists
which are in fact pure streams. So we can represent the previous example in a
streaming fashion:

```
  S.replicateM 10 (return 1)
    & S.zipWith (+) (S.fromList [1..10])
    & S.map (+1)
    & S.filter odd
    & S.sum
```

The good thing is that streamly's monadic streams are a generalization of pure
streams and therefore can represent pure streams just as easily, making lists a
special case.

## Pure, Streamed Operations

In this section we describe operations that consume and produce lists in a pure
context and can be composed to process them in a streaming fashion without
buffering them at any stage.  Streamly can be used as a pure list if the
underlying monad is the `Identity` monad.  The pure streaming APIs of streamly
and lists are identical except the fact that they use different constructors.

### Construction

Lists:

```
> "hello" : "world" : []
["hello","world"]
```

Streams:

```
import Streamly
import qualified Streamly.Prelude as S
import Streamly.Prelude ((.:), (|:))

> S.toList $ "hello" .: "world" .: S.nil
["hello","world"]
```

`nil` corresponds to `[]`, and `.:` corresponds to `:`.  `toList` converts a
stream to list.

### Pattern Matching

Lists:

```
TBD
```

Streams:

```
TBD
```

### Generation

Lists:

```
main = return (replicate 10 1) >>= print
```

[TBD] add a pure replicate API to streamly.

Streams:

```
main = S.toList (S.replicate 10 1) >>= print
```

### Transformation

Transformation using a pure function:

Lists:

```
main = return (map (+1) (replicate 10 1)) >>= print
```

Streams:

```
main = S.toList (S.map (+1) (S.replicate 10 1)) >>= print
```

### Foldable

Lists:

```
import Data.Foldable (length)
main = length (replicate 10 1)
```

Streams:

```
import Data.Foldable (length)
main = length (S.replicate 10 1 :: SerialT Identity Int)
```

## Monadic, Buffered Operations

Lists when used in a monadic context are non-streaming and therefore get
buffered, accumulating all the elements in memory.  All the list operations in
a monadic context are buffering.  Such list operations exist in
`Control.Monad`, `Data.Traversable`, and `Data.Foldable` in `base`.

These operations whenever they are provided in streamly are only for
compatibility with lists. They work fine on smaller lists but cannot scale well
to arbitrarily sized lists.  They are not recommended to be used in general,
instead use the equivalent monadic, streaming operations which are as easy to
use but always scale better.

### Generation

Lists:

```
main = replicateM 10 1 >>= print
```

### Transformation

Lists:

```
import Data.Traversable (mapM)
main = mapM print (replicate 10 1)
```

Streams:

```
import Data.Traversable (mapM)
main = mapM print (S.replicate 10 1 :: SerialT Identity Int)
```

### Folding

Lists:

foldrM :: (Foldable t, Monad m) => (a -> b -> m b) -> b -> t a -> m b

### Zipping

Lists:

```
zipWithM :: Applicative m => (a -> b -> m c) -> [a] -> [b] -> m [c]
```

## Nested, Buffered Operations

Operations that involve multiple layers of lists e.g. `lines`, `splitAt` and
`group` are not streaming. Each individual layer is buffered even though the
layers as a whole are streamed.

## Monadic, Streamed Operations

Streams provide streaming versions of the monadic, buffered list APIs. The
streaming versions of these operations run in constant memory in contrast to
the buffered operations.  Monadic APIs in streamly are suffixed with the letter
`M`.

### Construction

Unlike lists we can construct streams using monadic actions:

```
> S.toList $ getLine |: getLine |: S.nil
hello
world
["hello","world"]
```

`|:` is the monadic construction equivalent of `:`.
The monadic action `getLine` is executed twice in a sequence, the first one
gets "hello" and second one gets "world" from the terminal and a stream
consisting of those strings is produced.

## Generation

```
main = S.toList (S.replicateM 10 1) >>= print
```

### Folding

```
> runIdentity $ foldrM (\x xs -> return (x : xs)) [] (serially $ fromFoldable [1,2,3])
[1,2,3]
```

### Transformation

```haskell
import Control.Concurrent
import Streamly
import qualified Streamly.Prelude as S

func = S.mapM (\x -> threadDelay 1000000 >> print x) $ S.replicate 10 1
main = runStream func
```

## Nested, Streamed Operations

Streams provide streaming versions of the nested, buffered APIs in standard
lists.  TBD

## Concurrent Monadic Streaming

All the monadic streaming operations described in the previous section are
inherently concurrent.  Concurrent behavior can be enabled by just using an
appropriate modifying combinator, without any other change to the code.

The following code prints one element every second:

```haskell
import Control.Concurrent
import Streamly
import qualified Streamly.Prelude as S

func = S.mapM (\x -> threadDelay 1000000 >> print x) $ S.replicate 10 1
main = runStream func
```

To run it concurrently, just run the same code with `asyncly` combinator:

```haskell
main = runStream $ asyncly func
```

The `mapM` combinator now maps the monadic delay action concurrently on all the
stream elements.  It prints all the 10 elements after one second because all
the delay actions run concurrently. Alternatively we can write:

```
func = S.mapM $ asyncly $ S.replicateM (threadDelay 1000000 >> print 1)
main = runStream func
```

Here, the `replicateM` operation replicates the action concurrently.  In real
applications this could be a request to a web server that you may want to
perform multiple times concurrently.

## Streams obviate the need for difference lists

Lists are notorious for append performance. Difference lists have been invented
to avoid that problem. Streamly streams do not suffer from that problem, they
provide an O(1) append performance.

## Streams perform better than lists

Streamly utilizes stream fusion to provide excellent performance.  Here is a
performance comparison of pure lists and streamly streams:

TBD

## Differences from lists

Haskell lists can be treated as a special case of monadic streams using
_Identity_ as the underlying monad.  Streams perform as well as or even better
than lists. Therefore, you can fearlessly use streams where you need lists.
Currently there are exactly three known incompatibilities between lists and
stream APIs, everything else is identical:

1.  In standard lists we use `[]` and `:` constructors for both constructing
    and pattern matching. In streamly we use `nil` and `.:` for construction
    and `Nil` and `Cons` for pattern matching.
2.  Partial functions like `tail`, `head` from lists are implemented safely in
    streamly and therefore have incompatible signatures because they return a
    `Maybe`.
3.  We have not implemented the list-compatible non-streaming (buffered)
    operations, instead we recommend that you use the corresponding monadic
    streaming operations.

## Use streams instead of lists

By using monadic streams instead of lists you get three crucial benefits:

* The code is guaranteed to run in constant memory when you use streaming
  operations instead of the buffering operations.
* You can use a monadic actions anywhere in the code.  When using pure lists
  you may often realize that you have to print a warning message somewhere in
  the middle of processing, how do you do that in pure code without returning a
  value to some monadic context?  With monadic streams it is not a problem,
  they are already monadic, you can use IO or any other monadic code anywhere.
* You can make your code concurrent whenever and wherever you want.  It's just
  like using a magic spell!
* Your code never suffers from append performance issues.

## References

* [Data.List](https://hackage.haskell.org/package/base/docs/Data-List.html)
* [Control.Monad](http://hackage.haskell.org/package/base/docs/Control-Monad.html)
* [Data.Foldable](http://hackage.haskell.org/package/base/docs/Data-Foldable.html)
* [Data.Traversable](http://hackage.haskell.org/package/base/docs/Data-Traversable.html)
* [Difference lists](https://hackage.haskell.org/package/dlist)
