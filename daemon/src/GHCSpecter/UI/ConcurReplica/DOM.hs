{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module is originated from Concur.Replica.DOM and tailored
-- for IHTML.
module GHCSpecter.UI.ConcurReplica.DOM
  ( WidgetConstraints,
    el,
    elWithNamespace,
    text,
    div,
    table,
    thead,
    tbody,
    tr,
    trKeyed,
    th,
    td,
    tfoot,
    section,
    header,
    footer,
    button,
    form,
    p,
    s,
    ul,
    span,
    strong,
    li,
    liKeyed,
    h1,
    h2,
    h3,
    h4,
    h5,
    h6,
    hr,
    pre,
    input,
    label,
    a,
    mark,
    ruby,
    rt,
    rp,
    bdi,
    bdo,
    wbr,
    details,
    summary,
    menuitem,
    menu,
    fieldset,
    legend,
    datalist,
    optgroup,
    keygen,
    output,
    progress,
    meter,
    center,
    audio,
    video,
    source,
    track,
    embed,
    object,
    param,
    ins,
    del,
    small,
    cite,
    dfn,
    abbr,
    time,
    var,
    samp,
    kbd,
    caption,
    colgroup,
    col,
    nav,
    article,
    aside,
    address,
    main_,
    body,
    figure,
    figcaption,
    dl,
    dt,
    dd,
    img,
    iframe,
    canvas,
    math,
    select,
    option,
    textarea,
    sub,
    sup,
    br,
    ol,
    blockquote,
    code,
    em,
    i,
    b,
    u,
    q,
    script,
    link,
  )
where

import Concur.Core
  ( MonadSafeBlockingIO (liftSafeBlockingIO),
    MonadUnsafeBlockingIO (liftUnsafeBlockingIO),
    MultiAlternative,
    Widget,
    display,
    mapView,
    orr,
    wrapView,
  )
import Concur.Replica.DOM.Props (Prop (PropBool, PropEvent, PropMap, PropText), Props (Props), key)
import Control.Concurrent (newEmptyMVar, putMVar, takeMVar)
import Control.ShiftMap (ShiftMap (shiftMap))
import Data.Map qualified as M
import Data.Monoid ((<>))
import Data.Text qualified as T
import GHCSpecter.UI.ConcurReplica.Types (IHTML (..))
import Replica.VDOM (Attr (ABool, AEvent, AMap, AText), HTML, Namespace, VDOM (VNode, VText))
import Prelude hiding (div, span)

type WidgetConstraints m = (ShiftMap (Widget IHTML) m, Monad m, MonadSafeBlockingIO m, MonadUnsafeBlockingIO m, MultiAlternative m)

el :: forall m a. WidgetConstraints m => T.Text -> [Props a] -> [m a] -> m a
el = elWithNamespace Nothing

elWithNamespace :: forall m a. WidgetConstraints m => Maybe Namespace -> T.Text -> [Props a] -> [m a] -> m a
elWithNamespace mNamespace e attrs children = do
  attrs' <- liftUnsafeBlockingIO $ mapM toAttr attrs
  shiftMap
    (mapView (f (fmap fst attrs')))
    $ orr (children <> concatMap snd attrs')
  where
    f :: [(T.Text, Attr)] -> IHTML -> IHTML
    f xs (Update ys) = Update [VNode e (M.fromList xs) mNamespace ys]
    f _ NoUpdate = NoUpdate

    toAttr :: Props a -> IO ((T.Text, Attr), [m a])
    toAttr (Props k (PropText v)) = pure ((k, AText v), [])
    toAttr (Props k (PropBool v)) = pure ((k, ABool v), [])
    toAttr (Props k (PropEvent extract)) = do
      n <- newEmptyMVar
      pure ((k, AEvent $ putMVar n), [liftSafeBlockingIO (extract <$> takeMVar n)])
    toAttr (Props k (PropMap m)) = do
      m' <- mapM toAttr m
      pure ((k, AMap $ M.fromList $ fmap fst m'), concatMap snd m')

-- | @Concur.Replica.DOM.text@ was specialized to Widget HTML, so we reintroduced this @text@ for Widget IHTML.
text :: T.Text -> Widget IHTML a
text txt = display (Update [VText txt])

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/div
div :: WidgetConstraints m => [Props a] -> [m a] -> m a
div = el "div"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: WidgetConstraints m => [Props a] -> [m a] -> m a
table = el "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: WidgetConstraints m => [Props a] -> [m a] -> m a
thead = el "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: WidgetConstraints m => [Props a] -> [m a] -> m a
tbody = el "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: WidgetConstraints m => [Props a] -> [m a] -> m a
tr = el "tr"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr>
trKeyed :: WidgetConstraints m => T.Text -> [Props a] -> [m a] -> m a
trKeyed k props = el "tr" (key k : props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: WidgetConstraints m => [Props a] -> [m a] -> m a
th = el "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: WidgetConstraints m => [Props a] -> [m a] -> m a
td = el "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: WidgetConstraints m => [Props a] -> [m a] -> m a
tfoot = el "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: WidgetConstraints m => [Props a] -> [m a] -> m a
section = el "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: WidgetConstraints m => [Props a] -> [m a] -> m a
header = el "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: WidgetConstraints m => [Props a] -> [m a] -> m a
footer = el "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/button
button :: WidgetConstraints m => [Props a] -> [m a] -> m a
button = el "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: WidgetConstraints m => [Props a] -> [m a] -> m a
form = el "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: WidgetConstraints m => [Props a] -> [m a] -> m a
p = el "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: WidgetConstraints m => [Props a] -> [m a] -> m a
s = el "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: WidgetConstraints m => [Props a] -> [m a] -> m a
ul = el "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: WidgetConstraints m => [Props a] -> [m a] -> m a
span = el "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: WidgetConstraints m => [Props a] -> [m a] -> m a
strong = el "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: WidgetConstraints m => [Props a] -> [m a] -> m a
li = el "li"

-- | Contains `Key`, inteded to be used for child replacement patch
--
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
liKeyed :: WidgetConstraints m => T.Text -> [Props a] -> [m a] -> m a
liKeyed k props = el "li" (key k : props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: WidgetConstraints m => [Props a] -> [m a] -> m a
h1 = el "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: WidgetConstraints m => [Props a] -> [m a] -> m a
h2 = el "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: WidgetConstraints m => [Props a] -> [m a] -> m a
h3 = el "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: WidgetConstraints m => [Props a] -> [m a] -> m a
h4 = el "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: WidgetConstraints m => [Props a] -> [m a] -> m a
h5 = el "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: WidgetConstraints m => [Props a] -> [m a] -> m a
h6 = el "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: WidgetConstraints m => [Props a] -> m a
hr = flip (el "hr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/pre
pre :: WidgetConstraints m => [Props a] -> [m a] -> m a
pre = el "pre"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
input :: WidgetConstraints m => [Props a] -> m a
input = flip (el "input") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/label
label :: WidgetConstraints m => [Props a] -> [m a] -> m a
label = el "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: WidgetConstraints m => [Props a] -> [m a] -> m a
a = el "a"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: WidgetConstraints m => [Props a] -> [m a] -> m a
mark = el "mark"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: WidgetConstraints m => [Props a] -> [m a] -> m a
ruby = el "ruby"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: WidgetConstraints m => [Props a] -> [m a] -> m a
rt = el "rt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: WidgetConstraints m => [Props a] -> [m a] -> m a
rp = el "rp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: WidgetConstraints m => [Props a] -> [m a] -> m a
bdi = el "bdi"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: WidgetConstraints m => [Props a] -> [m a] -> m a
bdo = el "bdo"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: WidgetConstraints m => [Props a] -> m a
wbr = flip (el "wbr") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/details
details :: WidgetConstraints m => [Props a] -> [m a] -> m a
details = el "details"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: WidgetConstraints m => [Props a] -> [m a] -> m a
summary = el "summary"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: WidgetConstraints m => [Props a] -> [m a] -> m a
menuitem = el "menuitem"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: WidgetConstraints m => [Props a] -> [m a] -> m a
menu = el "menu"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: WidgetConstraints m => [Props a] -> [m a] -> m a
fieldset = el "fieldset"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: WidgetConstraints m => [Props a] -> [m a] -> m a
legend = el "legend"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: WidgetConstraints m => [Props a] -> [m a] -> m a
datalist = el "datalist"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: WidgetConstraints m => [Props a] -> [m a] -> m a
optgroup = el "optgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: WidgetConstraints m => [Props a] -> [m a] -> m a
keygen = el "keygen"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: WidgetConstraints m => [Props a] -> [m a] -> m a
output = el "output"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: WidgetConstraints m => [Props a] -> [m a] -> m a
progress = el "progress"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: WidgetConstraints m => [Props a] -> [m a] -> m a
meter = el "meter"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: WidgetConstraints m => [Props a] -> [m a] -> m a
center = el "center"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: WidgetConstraints m => [Props a] -> [m a] -> m a
audio = el "audio"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: WidgetConstraints m => [Props a] -> [m a] -> m a
video = el "video"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: WidgetConstraints m => [Props a] -> m a
source = flip (el "source") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/track
track :: WidgetConstraints m => [Props a] -> m a
track = flip (el "track") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/embed
embed :: WidgetConstraints m => [Props a] -> m a
embed = flip (el "embed") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: WidgetConstraints m => [Props a] -> [m a] -> m a
object = el "object"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: WidgetConstraints m => [Props a] -> m a
param = flip (el "param") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: WidgetConstraints m => [Props a] -> [m a] -> m a
ins = el "ins"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: WidgetConstraints m => [Props a] -> [m a] -> m a
del = el "del"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: WidgetConstraints m => [Props a] -> [m a] -> m a
small = el "small"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: WidgetConstraints m => [Props a] -> [m a] -> m a
cite = el "cite"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: WidgetConstraints m => [Props a] -> [m a] -> m a
dfn = el "dfn"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: WidgetConstraints m => [Props a] -> [m a] -> m a
abbr = el "abbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: WidgetConstraints m => [Props a] -> [m a] -> m a
time = el "time"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: WidgetConstraints m => [Props a] -> [m a] -> m a
var = el "var"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: WidgetConstraints m => [Props a] -> [m a] -> m a
samp = el "samp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: WidgetConstraints m => [Props a] -> [m a] -> m a
kbd = el "kbd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: WidgetConstraints m => [Props a] -> [m a] -> m a
caption = el "caption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: WidgetConstraints m => [Props a] -> [m a] -> m a
colgroup = el "colgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: WidgetConstraints m => [Props a] -> m a
col = flip (el "col") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/nav
nav :: WidgetConstraints m => [Props a] -> [m a] -> m a
nav = el "nav"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/article
article :: WidgetConstraints m => [Props a] -> [m a] -> m a
article = el "article"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: WidgetConstraints m => [Props a] -> [m a] -> m a
aside = el "aside"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: WidgetConstraints m => [Props a] -> [m a] -> m a
address = el "address"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: WidgetConstraints m => [Props a] -> [m a] -> m a
main_ = el "main"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: WidgetConstraints m => [Props a] -> [m a] -> m a
body = el "body"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: WidgetConstraints m => [Props a] -> [m a] -> m a
figure = el "figure"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: WidgetConstraints m => [Props a] -> [m a] -> m a
figcaption = el "figcaption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: WidgetConstraints m => [Props a] -> [m a] -> m a
dl = el "dl"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: WidgetConstraints m => [Props a] -> [m a] -> m a
dt = el "dt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: WidgetConstraints m => [Props a] -> [m a] -> m a
dd = el "dd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: WidgetConstraints m => [Props a] -> m a
img = flip (el "img") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: WidgetConstraints m => [Props a] -> [m a] -> m a
iframe = el "iframe"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: WidgetConstraints m => [Props a] -> [m a] -> m a
canvas = el "canvas"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: WidgetConstraints m => [Props a] -> [m a] -> m a
math = el "math"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: WidgetConstraints m => [Props a] -> [m a] -> m a
select = el "select"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: WidgetConstraints m => [Props a] -> [m a] -> m a
option = el "option"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: WidgetConstraints m => [Props a] -> [m a] -> m a
textarea = el "textarea"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: WidgetConstraints m => [Props a] -> [m a] -> m a
sub = el "sub"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: WidgetConstraints m => [Props a] -> [m a] -> m a
sup = el "sup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: WidgetConstraints m => [Props a] -> m a
br = flip (el "br") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ol
ol :: WidgetConstraints m => [Props a] -> [m a] -> m a
ol = el "ol"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/blockquote
blockquote :: WidgetConstraints m => [Props a] -> [m a] -> m a
blockquote = el "blockquote"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: WidgetConstraints m => [Props a] -> [m a] -> m a
code = el "code"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: WidgetConstraints m => [Props a] -> [m a] -> m a
em = el "em"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: WidgetConstraints m => [Props a] -> [m a] -> m a
i = el "i"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: WidgetConstraints m => [Props a] -> [m a] -> m a
b = el "b"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: WidgetConstraints m => [Props a] -> [m a] -> m a
u = el "u"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: WidgetConstraints m => [Props a] -> [m a] -> m a
q = el "q"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
script :: WidgetConstraints m => [Props a] -> [m a] -> m a
script = el "script"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
link :: WidgetConstraints m => [Props a] -> m a
link = flip (el "link") []
