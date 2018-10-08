# enum-categorical-digraphs

So we have a directed graph (digraph) that we want to represent a category.

- All vertices must have at least one self-loop (`id`)
- Composition of arcs is an arc: `e(a,b) && e(b, c) => e(a, c)`

We want to enumerate all digraphs of a given number of vertices that represent categories in this way.

We represent partially determined digraphs as follows:

```haskell
data PdGraph = PdGraph
  { numVertices :: !Int -- ^ The number of vertices
  , adjMatrix :: !(Matrix Int) -- ^ Diagonal is 1, 0 -> undetermined, 1 -> arc, 2 -> anti-arc
  , undeterminedArcs :: Set Arc
  , neighborhoods :: IntMap Neighborhood -- ^ The neighborhoods of each vertex
  } deriving (Eq, Show, Generic)
```

At each step of the enumeration:
- We choose an undetermined arc
- We pick either determination (arc or anti-arc)
  + An anti-arc is an ordered pair `v1, v2` such that the final digraph does
    _not_ have any arc from `v1` to `v2`
- Finally, we propagate the results of our choice to update the determinations
  in our partially-determined digraph

This provides a mapping from `PdGraph`s where all choices have been totally
propagated, i.e. we've applied the category constraints on the digraph to
make as many determinations as possible.

Thus all of the final results (completely determined digraphs)
should represent valid categories.


## References

See [Structural aspects of semigroups based on digraphs](https://arxiv.org/pdf/1704.00937.pdf)
on arxiv for an encoding of semgroups as digraphs.

From the abstract:

```
In this paper, we consider the question of when there is a transformation in <D> containing
a large cycle, and, for fixed k ∈ N, we give a linear time algorithm to verify if <D> contains a
transformation with a cycle of length k. We also classify those digraphs D such that <D> has one of
the following properties: inverse, completely regular, commutative, simple, 0-simple, a semilattice,
a rectangular band, congruence-free, is K -trivial or K -universal where K is any of Green’s H -,
L -, R-, or J -relation, and when <D> has a left, right, or two-sided zero.
```


# Docs

Haddock generated documentation may be found [here](https://michaeljklein.github.io/enum-categorical-digraphs/)

