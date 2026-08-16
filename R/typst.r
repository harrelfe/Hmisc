## -----------------------------------------------------------------------
## typstTranslate: translate arbitrary character strings (variable
## labels, titles, etc.) so they display correctly as literal text in
## Typst markup, with optional math-mode handling for superscripts and
## Greek letter names -- the Typst analog of latexTranslate().
##
## Deliberately simpler than latexTranslate() in several ways:
##
##  - Built entirely on vectorized base gsub() calls, not sedit()/
##    replace.substring.wild(); the Greek-letter pass in particular
##    collapses latexTranslate's 39-iteration loop into one regex
##    alternation.
##
##  - Several characters latexTranslate escapes (|, %, &, >, the pound
##    sign) have NO special meaning in Typst markup and are simply left
##    alone here -- carrying over LaTeX's escaping list unchanged would
##    have added noise, not correctness.
##
##  - Where LaTeX requires math mode for symbols like \leq/\geq (they
##    don't exist in text mode at all), Typst can just use the literal
##    Unicode character in ordinary text -- no $...$ wrapping needed,
##    so <= and >= translate directly to unicode rather than opening a
##    math span the way latexTranslate must.
##
##  - Superscripts are always parenthesized ($^(2)$, not $^2$) -- valid
##    for both single- and multi-character exponents, so there's no need
##    for latexTranslate's length-conditional/math-mode-tracking logic.
##
##  - $ protection uses an actual unlikely-collision control character
##    (\u0001) as the placeholder rather than a text token like
##    latexTranslate's "DOLLARS", which could in principle already
##    appear verbatim in real input text.
##
## Characters that ARE escaped, because they're syntactically meaningful
## in Typst markup mode and could otherwise corrupt output or (in the
## case of '<') cause an outright compile error: # < _ @ ` * ~ and \
## itself. '<' specifically needs this because Typst reads a bare '<' as
## the start of label-reference syntax (<name>) -- confirmed directly:
## an unescaped P-value formatted as "<0.001" breaks Typst compilation
## with "unclosed label" if not escaped.
##
## A second confirmed compile-breaking case, now handled: when a Greek
## substitution and a superscript substitution land immediately next to
## each other with no text between them (e.g. "chi^2" -> naively
## "$chi$$^(2)$"), Typst reads the touching $$ as an EMPTY math block
## followed by bare (invalid, out-of-math-mode) text -- a real
## "unexpected hat" compile error, not just a cosmetic double-span. The
## cleanup pass near the end of this function merges any such touching
## spans into one continuous span before they can cause this.
##
## Not carried over from latexTranslate: the `pb` argument (auto-sizing
## \left/\right delimiters) and the `inn`/`out` extension arguments --
## Typst doesn't need LaTeX's delimiter-sizing workaround for ordinary
## text-mode brackets, and the extension hooks weren't asked for. Add
## back if a concrete need for either turns up.
## -----------------------------------------------------------------------


#' Translate a Character String for Typst Markup
#'
#' Translates arbitrary character strings -- variable labels, titles,
#' captions, and similar free text -- so they display correctly as
#' literal text in Typst markup, with optional math-mode handling for
#' superscripts and Greek letter names. This is the Typst analog of
#' \code{\link{latexTranslate}} and \code{\link{htmlTranslate}}.
#'
#' \code{typstTranslate} is built entirely from vectorized base
#' \code{\link{gsub}} calls rather than \code{\link{sedit}}, and is
#' deliberately simpler than \code{latexTranslate} in several respects:
#'
#' \itemize{
#'   \item Several characters \code{latexTranslate} escapes (\code{|},
#'     \code{\%}, \code{&}, \code{>}, the pound sign) have no special
#'     meaning in Typst markup and are left untouched here -- carrying
#'     over LaTeX's escaping list unchanged would add noise, not
#'     correctness.
#'   \item Where LaTeX requires math mode for symbols such as
#'     \code{\\leq}/\code{\\geq} (they do not exist in text mode at
#'     all), Typst can use the literal Unicode character directly in
#'     ordinary text, so \code{<=} and \code{>=} translate straight to
#'     Unicode rather than opening a math span.
#'   \item Superscripts (\verb{^digits}) are rendered with Typst's
#'     text-mode \verb{#super[]} function rather than a bare math-mode
#'     span. A bare \verb{$^(3)$} span with no base inside the same
#'     span is invalid Typst syntax ("unexpected hat") unless something
#'     happens to merge a base into it, and even when made syntactically
#'     valid, pulling an ordinary word into math mode causes Typst to
#'     auto-italicize/treat it as bare variable letters rather than
#'     showing plain upright text. \verb{#super[]} avoids both problems:
#'     it is valid immediately after any preceding content (or none),
#'     and does not restyle whatever precedes it.
#'   \item Greek letter names are recognized as whole words (via a
#'     single vectorized regex alternation, not a per-letter loop) and
#'     wrapped in \verb{$...$} so they render as the actual Greek
#'     glyph.
#' }
#'
#' Characters that \emph{are} escaped, because they are syntactically
#' meaningful in Typst markup mode and could otherwise corrupt output or
#' (in the case of \verb{<}) cause an outright compile error, are
#' \verb{# < _ @ `} \verb{*} \verb{~} and the backslash itself.
#' \verb{<} specifically needs this because Typst reads a bare \verb{<}
#' as the start of label-reference syntax (\verb{<name>}); an unescaped
#' value such as a P-value formatted as \verb{"<0.001"} will otherwise
#' break Typst compilation with an "unclosed label" error.
#' \verb{_} and \verb{*} are escaped unconditionally (not only where
#' they would definitely cause trouble) because ordinary variable names
#' and labels routinely contain underscores or asterisks, and Typst's
#' emphasis/strong-text shorthand can trigger across unrelated stretches
#' of text if these are left unescaped.
#'
#' Math spans that end up touching with no text between them (which can
#' happen, for example, when a Greek-letter substitution lands
#' immediately next to another math-producing substitution) are merged
#' into one continuous span. Typst reads two adjacent \verb{$...$$...$}
#' spans as an \emph{empty} math block followed by bare text -- a real
#' compile error ("unexpected hat"), not merely a cosmetic issue -- so
#' this merge is applied unconditionally wherever it is safe to do so.
#'
#' Not carried over from \code{latexTranslate}: the \code{pb} argument
#' (auto-sizing \verb{\\left}/\verb{\\right} delimiters) and the
#' \code{inn}/\code{out} extension arguments -- Typst does not need
#' LaTeX's delimiter-sizing workaround for ordinary text-mode brackets,
#' and the extension hooks were not needed for current use cases.
#'
#' @param object A character vector, or an object coercible to one via
#'   \code{\link{as.character}}, to be translated for display in Typst
#'   markup.
#' @param greek Logical. If \code{TRUE}, whole-word matches of Greek
#'   letter names (\code{alpha}, \code{beta}, \code{Gamma}, etc.) are
#'   wrapped in Typst math mode so they render as the actual Greek
#'   glyph rather than the literal English name. Default is
#'   \code{FALSE}.
#' @param na Character string to substitute for \code{NA} elements of
#'   \code{object} before translation. Default is \code{''}.
#' @param ... Currently unused; present for signature compatibility
#'   with \code{\link{latexTranslate}}/\code{\link{htmlTranslate}}.
#'
#' @return A character vector of the same length as \code{object},
#'   containing valid Typst markup safe for direct insertion into Typst
#'   source (for example, inside a \verb{```{=typst}} raw block, or as
#'   the content of a native \verb{#table()} cell).
#'
#' @seealso \code{\link{latexTranslate}}, \code{\link{htmlTranslate}},
#'   \code{\link{typstAsis}}
#'
#' @examples
#' typstTranslate("P<0.001")                     # escapes the literal <
#' typstTranslate("Volume (cm^3)")                # #super[] superscript
#' typstTranslate("rho coefficient", greek=TRUE)  # real Greek rho glyph
#' typstTranslate("chi^2 statistic", greek=TRUE)  # merged math span
#'
#' @export

typstTranslate <- function(object, greek = FALSE, na = '', ...)
{
  text <- ifelse(is.na(object), na, as.character(object))

  ## Protect literal $ before any of the steps below insert their own
  ## math-mode $ delimiters
  text <- gsub('$', '\u0001', text, fixed = TRUE)

  ## Multi-character sequences with no Typst syntax of their own -- just
  ## become their literal Unicode characters, no math mode required
  text <- gsub('<=', '\u2264', text, fixed = TRUE)
  text <- gsub('>=', '\u2265', text, fixed = TRUE)

  ## Escape existing backslashes first, so the escaping below doesn't
  ## double up on backslashes it just inserted
  text <- gsub('\\', '\\\\', text, fixed = TRUE)

  ## Characters syntactically special in Typst markup mode
  specials <- c('#', '<', '_', '@', '`', '*', '~')
  for (ch in specials)
    text <- gsub(ch, paste0('\\', ch), text, fixed = TRUE)

  ## ^digits -> Typst text-mode superscript via #super[], NOT math mode.
  ## Math mode ($^(...)$) was tried first but has two real problems:
  ## (1) a bare "$^(3)$" math span with no preceding base INSIDE that
  ## same span is invalid Typst syntax ("unexpected hat") unless
  ## something happens to merge a base into it -- this only accidentally
  ## worked for a Greek-letter base (via the $$-merge step below), never
  ## for an ordinary word like "cm" in "cm^3"; (2) even made valid,
  ## pulling a word like "cm" into math mode makes Typst auto-italicize/
  ## treat it as bare variable letters (read as c times m) rather than
  ## showing plain upright unit text. #super[] sidesteps both: it's a
  ## text-mode function, valid after any preceding content or none, and
  ## doesn't restyle whatever precedes it.
  text <- gsub('\\^(-?[0-9]+)', '#super[\\1]', text, perl = TRUE)

  if (greek) {
    gl <- c('alpha','beta','gamma','delta','epsilon','varepsilon','zeta',
            'eta','theta','vartheta','iota','kappa','lambda','mu','nu',
            'xi','pi','varpi','rho','varrho','sigma','varsigma','tau',
            'upsilon','phi','varphi','chi','psi','omega','Gamma','Delta',
            'Theta','Lambda','Xi','Pi','Sigma','Upsilon','Phi','Psi','Omega')
    pat <- paste0('\\b(', paste(gl, collapse = '|'), ')\\b')
    text <- gsub(pat, '$\\1$', text, perl = TRUE)
  }

  ## Merge math spans our own substitutions could still leave touching
  ## with no text between them into one continuous span. With
  ## superscripts now using #super[] instead of math mode (see above),
  ## the only remaining source of $...$ spans is the Greek-word pass,
  ## and two separate Greek-word matches can never end up touching (a
  ## word boundary always separates them) -- so this is now a defensive
  ## fallback rather than something load-bearing for a known case, kept
  ## in place since it's cheap and harmless if it never fires. Safe to
  ## collapse unconditionally: literal user $ characters are still
  ## behind the \u0001 placeholder at this point.
  text <- gsub('$$', '', text, fixed = TRUE)

  gsub('\u0001', '\\$', text, fixed = TRUE)
}


## -----------------------------------------------------------------------
## typstFunctions: Typst utility function definitions, stored as R
## character strings, for use by typst.X print/render methods (in place
## of requiring users to install a local Typst package such as
## htypstmisc). Each element is emitted once per document via
## typstAsis(typstFunctions$name) before any calls to that function are
## emitted, the same way htypstmisc's spikehist() was compile-tested.
##
## Usage from a typst.X method:
##   typstAsis(typstFunctions$spikehist)   # define, once per document
##   typstAsis(paste0("#spikehist((", paste(heights, collapse=", "), "))"))
## -----------------------------------------------------------------------

typstFunctions <- list(

  ## spikehist: spike histogram, the Typst analog of the LaTeX `picture`
  ## environment used by latex.describe.single. Confirmed by
  ## compile-testing: height=0.2cm / baseline-shift=auto (resolving to
  ## -height) seats the spikes on the baseline of the current text line
  ## when placed inline next to running text -- the intended use in
  ## typst.describe.single.
  ##
  ## heights: array of non-negative numbers, one per bin. Zero-height
  ##          bins are legal (drawn as nothing) -- describe() output
  ##          routinely has sparse bins at the tails of a distribution.
  ## width, height: overall bounding box (Typst length, e.g. 3cm, 30pt).
  ## stroke-width: thickness of each spike.
  ## baseline-shift: defaults to auto, resolved in the function body to
  ##          -height (Typst doesn't allow a parameter default to
  ##          reference a sibling parameter directly -- confirmed by
  ##          compile error -- hence the auto-sentinel + body-resolution
  ##          pattern rather than `baseline-shift: -height` directly in
  ##          the parameter list).
  spikehist = '
#let spikehist(
  heights,
  width: 3cm,
  height: 0.2cm,
  stroke-width: 0.5pt,
  baseline-shift: auto
) = {
  let bshift = if baseline-shift == auto { -height } else { baseline-shift }
  let n = heights.len()
  let max-h = calc.max(..heights)
  box(width: width, height: height, baseline: height + bshift)[
    #for (i, h) in heights.enumerate() {
      if h > 0 {
        let x = width * (i + 0.5) / n
        let h-scaled = height * h / max-h
        place(dx: x, dy: height - h-scaled,
          line(length: h-scaled, angle: 90deg, stroke: stroke-width))
      }
    }
  ]
}
'

  ## Additional typstFunctions elements go here as they're written, one
  ## per Typst utility function, following the same pattern.
)


## -----------------------------------------------------------------------
## typstAsis: canonical helper for emitting raw Typst markup from any
## typst.X print/render method (typst.describe, typst.describe.single,
## a future typst branch in prModFit, etc.).
##
## Wraps x in a Pandoc raw-block fence and marks it for knitr's asis
## treatment when running under Quarto/knitr, so no chunk-level
## `results='asis'` option is ever needed at the call site. Falls back to
## plain console output (cat()) when not running under knitr, so typst.X
## functions remain usable interactively while developing/testing.
##
## No knit_print method registration is required anywhere for this to
## work correctly -- confirmed empirically: print.describe (and any
## other print.X method) can call typst.describe(x), which returns
## typstAsis(...), and this renders correctly as real typeset Typst
## content with print.describe's existing prType()-based dispatch shape
## completely unchanged.
##
## This is deliberately NOT a markupSpecs$typst list entry -- it's
## format-agnostic plumbing (the fenced-raw-block/asis_output mechanism
## itself), not a per-format translation like the rest of
## markupSpecs$typst (varlabel, hfill, chisq, etc.).
## -----------------------------------------------------------------------


#' Emit Raw Typst Markup Without Requiring \code{results='asis'}
#'
#' Marks a character string as raw Typst source so that Quarto/knitr
#' renders it directly -- as real typeset Typst content -- rather than
#' displaying it as literal escaped text. This lets \code{print}/render
#' methods that build Typst output (for example \code{typst_describe},
#' \code{typst_describe_single}, or the Typst branch of a
#' \code{print.X}/\code{latex.X} method) return their result normally,
#' with no \code{results='asis'} chunk option required anywhere in the
#' calling document.
#'
#' \code{typstAsis} wraps \code{x} in a Pandoc raw-block fence
#' (\verb{````{=typst}} ... \verb{````}) and, when running inside a
#' knitr/Quarto render, returns the result via
#' \code{\link[knitr]{asis_output}}. Returning an \code{asis_output}
#' object as the visible top-level value of a chunk is what knitr's own
#' \code{knit_print} dispatch recognizes as content to insert verbatim
#' rather than escape -- this is the mechanism, confirmed empirically,
#' that requires no new \code{knit_print} method to be registered for
#' any particular class: any function that ultimately returns
#' \code{typstAsis(...)} as its result renders correctly, regardless of
#' how deeply the string was built up inside helper functions.
#'
#' The fence uses \strong{four} backticks, not the usual three. This is
#' deliberate: if \code{x} itself contains a triple-backtick sequence
#' (for example, output from a helper that wraps captured
#' \code{\link{print}} output in its own raw block, such as
#' \code{typst_verbatim}), an ordinary three-backtick outer fence would
#' be closed prematurely by that inner sequence, silently corrupting
#' everything in the document that follows -- confirmed directly as the
#' cause of a real rendering failure during development. Pandoc's
#' fenced-block matching only closes a fence with another fence of at
#' least the same length, so a four-backtick outer fence cannot be
#' broken by an inner three-backtick one.
#'
#' When not running inside a knitr render (for example, when called
#' interactively at the R console while developing or testing a
#' \code{typst_*} function), \code{typstAsis} falls back to printing
#' \code{x} via \code{\link{cat}} and returning it invisibly, so
#' \code{typst_*} functions remain directly usable outside Quarto
#' documents.
#'
#' \strong{Important:} \code{x} must already be genuine, complete Typst
#' syntax. \code{typstAsis} does not translate or interpret its
#' argument in any way -- it only marks already-finished Typst markup
#' so Pandoc passes it through untouched. This means \code{typstAsis}
#' must \emph{not} be used to wrap LaTeX-syntax math content that is
#' meant to be parsed by Pandoc's own markdown reader and translated to
#' Typst via texmath (for example, the equation content built by
#' \code{latexrms} and its calling \code{latex.X} methods) -- wrapping
#' such content in a \verb{{=typst}} raw block prevents Pandoc from ever
#' recognizing it as math to translate, and it will be inserted
#' unchanged, as literal LaTeX syntax, into a Typst document. That kind
#' of content should instead be returned via a bare
#' \code{\link[knitr]{asis_output}} call, with no raw-block fence, so
#' Pandoc's markdown/math parsing sees it directly.
#'
#' @param x A character string (or vector, coerced via
#'   \code{\link{paste0}}) containing complete, valid Typst markup.
#'
#' @return If called during a knitr/Quarto render, an object of class
#'   \code{knit_asis} (see \code{\link[knitr]{asis_output}}), which
#'   knitr inserts into the rendered document verbatim. Otherwise, the
#'   fenced string is printed to the console via \code{\link{cat}} and
#'   returned invisibly.
#'
#' @seealso \code{\link{typstTranslate}}, \code{\link[knitr]{asis_output}}
#'
#' @examples
#' \dontrun{
#' # Inside a print/render method's typst branch:
#' typst_describe <- function(object, ...) {
#'   R <- c(typstFunctions$spikehist, ...)  # build up Typst content
#'   typstAsis(paste(R, collapse = '\n\n'))
#' }
#'
#' # NOT appropriate: LaTeX-syntax math meant for texmath translation
#' # should use bare knitr::asis_output(), never typstAsis()
#' }
#'
#' @export
typstAsis <- function(x) {
  x <- paste0("````{=typst}\n", x, "\n````\n")

  if (length(getOption('knitr.in.progress')))
    knitr::asis_output(x)
  else {
    cat(x)
    invisible(x)
  }
}
