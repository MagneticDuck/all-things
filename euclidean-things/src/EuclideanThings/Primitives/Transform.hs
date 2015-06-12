-- module description -- {{{
{- |
  This module exports an interesting little definition of a
  transformation of euclidean space. It contains all isometric
  transformations and allows scaling along arbitrary lines.
  Any transformation defined here is easily invertible as
  long as it at no point scales by 0; if it scales by 0,
  its inverse will contain a scaling by \"Infinity\" and
  anything could happen! :D
-} -- }}}
module EuclideanThings.Primitives.Transform
-- exports -- {{{
  ( 
  -- * Constructors 
  Transform
  , transformFromReflect
  , transformFromTranslate
  , transformFromRotate
  , transformFromRotateAbout
  , transformFromScale
  , transformFromCompose 
  , transformFromScaleX
  , transformFromScaleY

  -- * Inversion
  , invertTransform

  -- * Application
  , transformPoint
  , transformLine
  , transformCircle 

  -- * Parity
  , transformIsEven ) where -- }}}

-- imports -- {{{
import EuclideanThings.Primitives.Vector
import EuclideanThings.Primitives.Angle
import EuclideanThings.Primitives.LineAndCircle -- }}}

-- * Constructors -- {{{
-- | Represents a transformation of the 2d plane. Any isometric
-- transformation can be represented in addition to any scaling.
data Transform a = 
  ScaleTransform (Line a) a
  | ComposeTransforms [Transform a] 

-- | Constructs a transformation representing reflection across
-- the supplied line.
transformFromReflect :: (RealFloat a) => Line a -> Transform a 
transformFromReflect = flip ScaleTransform (-1) 

-- | Constructs a transformation representing a translation
-- by the supplied vector
transformFromTranslate :: (RealFloat a) => Vector a -> Transform a 
transformFromTranslate v = 
  ComposeTransforms
    [ transformFromReflect line1
    , transformFromReflect line2 ]
  where
    line1 = lineFromPointAngle nullVector $ applyAngle (+90) $ angleFromVector v
    line2 = lineFromPointAngle (scaleVector (1/2) v) (applyAngle (+90) $ angleFromVector v)  

-- | Constructs a transformation representing a rotation by the
-- supplied delta angle about the origin
transformFromRotate  :: (RealFloat a) => Angle a -> Transform a 
transformFromRotate a =
  ComposeTransforms
    [ transformFromReflect line1
    , transformFromReflect line2 ]
  where
    line1 = lineFromPointAngle nullVector (angleFromDegree 0)
    line2 = lineFromPointAngle nullVector (applyAngle (/2) a) 

-- | Constructs a transformation representing a rotation by the
-- supplied angle about a supplied point
transformFromRotateAbout :: (RealFloat a) => Point a -> Angle a -> Transform a 
transformFromRotateAbout  c a =
  ComposeTransforms
    [ transformFromTranslate (-c)
    , transformFromRotate a
    , transformFromTranslate c ] 

-- docs -- {{{
{- |
  Constructs a transformation representing a multiplication
  of all x coordinates by the supplied scalar

  WARNING: If you intend to invert your transformation remember
  that the magnitude cannot be 0.
-} -- }}}
transformFromScaleX :: (RealFloat a) => a -> Transform a 
transformFromScaleX =
  ScaleTransform (lineFromPointPoint (Vector 0 0) (Vector 0 1)) 

-- docs -- {{{
{- |
  Constructs a transformation representing a multiplication
  of all y coordinates by the supplied scalar

  WARNING: If you intend to invert your transformation remember
  that the magnitude cannot be 0.
-} -- }}}
transformFromScaleY :: (RealFloat a) => a -> Transform a 
transformFromScaleY =
  ScaleTransform (lineFromPointPoint (Vector 0 0) (Vector 1 0)) 

-- docs -- {{{
{- |
  Constructs a transformation representing a scalar multiplication
  of the supplied magnitude.

  WARNING: If you intend to invert your transformation remember
  that the magnitude cannot be 0.
-} -- }}}
transformFromScale :: (RealFloat a) => a -> Transform a 
transformFromScale s = 
  ComposeTransforms 
    [ transformFromScaleX s
    , transformFromScaleY s ] 

-- | Constructs a transformation from the composition of a list 
-- of transformations, the head of the list being applied first. 
transformFromCompose :: [Transform a] -> Transform a 
transformFromCompose = ComposeTransforms 

transformFromPointTuples :: (RealFloat a) => (Point a, Point a) -> (Point a, Point a) -> Transform a
transformFromPointTuples (p1,p2) (p3,p4) =
  let
    trans1 = -- scaling
      transformFromScale $
        lengthFromVector (p4 - p3) / lengthFromVector (p2 - p1)
    [p11, p21] = ($ [p1, p2]) $ map $ transformPoint trans1

    trans2 = -- translation
      transformFromTranslate $ p11 - p3
    [p12, p22] = ($ [p1, p2]) $ map $ transformPoint trans1 -- why do I do this

    trans3 = -- rotation
      transformFromRotate (angleFromPoints (p22, p3, p4))
  in
    transformFromCompose [trans1, trans2, trans3]

 -- }}}

-- * Inversion -- {{{
-- | Constructs a transformation that is the inverse of the
-- supplied transformation; an inverse always exists unless
-- the transformation in question at any point scales by 0.
invertTransform :: (RealFloat a) => Transform a -> Transform a 
invertTransform (ScaleTransform l s) = ScaleTransform l (1/s)
invertTransform (ComposeTransforms ts) = ComposeTransforms $ reverse (map invertTransform ts) 
 -- }}}

-- * Application  -- {{{
-- | Applies a transformation to a point
transformPoint :: (RealFloat a) => Transform a -> Point a -> Point a 
transformPoint (ScaleTransform l s) p =
  let proj = projectLinePoint l p in proj + scaleVector s (p - proj)
transformPoint (ComposeTransforms ts) p =
  foldl (.) id (map transformPoint $ reverse ts) p 

-- | Applies a transformation to a line
transformLine ::  (RealFloat a) => Transform a -> Line a -> Line a 
transformLine t (Line p1 p2) = 
  Line (ts p1) (ts p2)
  where ts = transformPoint t 

-- | Applies a transformation to a circle by transforming
-- its center; this doesn't do anything exciting.
transformCircle ::  (RealFloat a) => Transform a -> Circle a -> Circle a 
transformCircle t (Circle c r) =
  Circle (transformPoint t c) r 

transformReflects :: (RealFloat a) => Transform a -> Integer 
transformReflects (ComposeTransforms ts) = sum (map transformReflects ts)
transformReflects (ScaleTransform _ x) =
  case signum x of
    -1 -> 1
    _ -> 0 

 -- }}}
 
-- * Parity -- {{{
-- | Counts the number of negative scales executed by a
-- transformation and returns whether it is even;
-- if it is odd, angles are "inverted" upon transformation.
-- The parity of a transformation is often useful to know. 
transformIsEven :: (RealFloat a) => Transform a -> Bool 
transformIsEven = even . transformReflects 
 -- }}}
