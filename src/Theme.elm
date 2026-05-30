module Theme exposing
    ( SpriteSource(..)
    , Background(..)
    , ParallaxLayer
    , JobVisual
    , JobVisualState(..)
    , Vocabulary
    , Theme2D
    )

{-| Visual skin types for the 2D SVG renderer.

A `Theme2D` is a pure record of rendering functions — the engine never
knows a theme exists.  Swap themes by constructing a different `Theme2D`
value; no simulation logic changes.
-}

import Id exposing (NodeID, QueueID)
import Job exposing (Job)
import Metrics exposing (NodeMetrics, QueueMetrics)
import Node exposing (NodeData)
import Queue exposing (Queue)
import Svg exposing (Svg)


-- ── Sprite sources ────────────────────────────────────────────────────────────

type SpriteSource
    = VectorSymbol String
      -- ^ SVG <symbol> id defined in <defs>; stamped with <use>
    | RasterImage String
      -- ^ URL to a PNG/WebP image
    | SpriteSheet
        { url : String
        , x   : Int
        , y   : Int   -- top-left of the clip rect (pixels)
        , w   : Int
        , h   : Int
        }


-- ── Backgrounds ───────────────────────────────────────────────────────────────

type Background
    = SolidColour String
      -- ^ CSS colour string
    | SVGPattern String
      -- ^ <pattern> id defined in defs
    | ImageBackground
        { url     : String
        , opacity : Float   -- 0.0..1.0; tint over the image
        }
    | ParallaxBackground (List ParallaxLayer)


type alias ParallaxLayer =
    { url     : String
    , depth   : Float   -- 0.0 = screen-fixed; 1.0 = moves 1:1 with camera
    , opacity : Float
    , tileX   : Bool
    , tileY   : Bool
    }


-- ── Job visual state ──────────────────────────────────────────────────────────

type alias JobVisual =
    { x     : Float
    , y     : Float
    , scale : Float          -- 1.0 in transit; smaller when packed in a queue
    , state : JobVisualState
    }


type JobVisualState
    = InQueue    Int    -- slot index (0 = front)
    | InTransit  Float  -- 0.0 just left source → 1.0 arrived
    | AtWorker          -- being processed; theme may add a progress ring
    | AwaitingSignoff   -- worker blocked, approver en route


-- ── Vocabulary ────────────────────────────────────────────────────────────────

type alias Vocabulary =
    { job    : String   -- "customer", "ship", "packet", "story"
    , worker : String   -- "cashier", "dock worker", "core", "developer"
    , queue  : String   -- "queue", "berth", "buffer", "backlog"
    , boss   : String   -- "manager", "bosmang", "scheduler"
    }


-- ── Theme2D ───────────────────────────────────────────────────────────────────

{-| A complete 2D SVG theme.  Each field is a rendering function; none of
them have side effects — they just produce SVG nodes.
-}
type alias Theme2D msg =
    { background    : Background
    , defs          : Svg msg
      -- ^ SVG <defs> block: symbols, patterns, filters defined once
    , jobSprite     : Job -> SpriteSource
    , nodeSprite    : NodeID -> NodeData -> SpriteSource
    , nodeView      : NodeID -> NodeData -> NodeMetrics -> Svg msg
    , queueView     : QueueID -> Queue -> QueueMetrics -> Svg msg
    , jobView       : Job -> JobVisual -> Svg msg
    , sparklineView : NodeID -> NodeMetrics -> Svg msg
    , vocab         : Vocabulary
    }
