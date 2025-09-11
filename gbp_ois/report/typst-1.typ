// Some definitions presupposed by pandoc's typst output.
#let blockquote(body) = [
  #set text( size: 0.92em )
  #block(inset: (left: 1.5em, top: 0.2em, bottom: 0.2em))[#body]
]

#let horizontalrule = [
  #line(start: (25%,0%), end: (75%,0%))
]

#let endnote(num, contents) = [
  #stack(dir: ltr, spacing: 3pt, super[#num], contents)
]

#show terms: it => {
  it.children
    .map(child => [
      #strong[#child.term]
      #block(inset: (left: 1.5em, top: -0.4em))[#child.description]
      ])
    .join()
}

// Some quarto-specific definitions.

#show raw.where(block: true): block.with(
    fill: luma(230), 
    width: 100%, 
    inset: 8pt, 
    radius: 2pt
  )

#let block_with_new_content(old_block, new_content) = {
  let d = (:)
  let fields = old_block.fields()
  fields.remove("body")
  if fields.at("below", default: none) != none {
    // TODO: this is a hack because below is a "synthesized element"
    // according to the experts in the typst discord...
    fields.below = fields.below.amount
  }
  return block.with(..fields)(new_content)
}

#let empty(v) = {
  if type(v) == "string" {
    // two dollar signs here because we're technically inside
    // a Pandoc template :grimace:
    v.matches(regex("^\\s*$")).at(0, default: none) != none
  } else if type(v) == "content" {
    if v.at("text", default: none) != none {
      return empty(v.text)
    }
    for child in v.at("children", default: ()) {
      if not empty(child) {
        return false
      }
    }
    return true
  }

}

#show figure: it => {
  if type(it.kind) != "string" {
    return it
  }
  let kind_match = it.kind.matches(regex("^quarto-callout-(.*)")).at(0, default: none)
  if kind_match == none {
    return it
  }
  let kind = kind_match.captures.at(0, default: "other")
  kind = upper(kind.first()) + kind.slice(1)
  // now we pull apart the callout and reassemble it with the crossref name and counter

  // when we cleanup pandoc's emitted code to avoid spaces this will have to change
  let old_callout = it.body.children.at(1).body.children.at(1)
  let old_title_block = old_callout.body.children.at(0)
  let old_title = old_title_block.body.body.children.at(2)

  // TODO use custom separator if available
  let new_title = if empty(old_title) {
    [#kind #it.counter.display()]
  } else {
    [#kind #it.counter.display(): #old_title]
  }

  let new_title_block = block_with_new_content(
    old_title_block, 
    block_with_new_content(
      old_title_block.body, 
      old_title_block.body.body.children.at(0) +
      old_title_block.body.body.children.at(1) +
      new_title))

  block_with_new_content(old_callout,
    new_title_block +
    old_callout.body.children.at(1))
}

#show ref: it => locate(loc => {
  let target = query(it.target, loc).first()
  if it.at("supplement", default: none) == none {
    it
    return
  }

  let sup = it.supplement.text.matches(regex("^45127368-afa1-446a-820f-fc64c546b2c5%(.*)")).at(0, default: none)
  if sup != none {
    let parent_id = sup.captures.first()
    let parent_figure = query(label(parent_id), loc).first()
    let parent_location = parent_figure.location()

    let counters = numbering(
      parent_figure.at("numbering"), 
      ..parent_figure.at("counter").at(parent_location))
      
    let subcounter = numbering(
      target.at("numbering"),
      ..target.at("counter").at(target.location()))
    
    // NOTE there's a nonbreaking space in the block below
    link(target.location(), [#parent_figure.at("supplement") #counters#subcounter])
  } else {
    it
  }
})

// 2023-10-09: #fa-icon("fa-info") is not working, so we'll eval "#fa-info()" instead
#let callout(body: [], title: "Callout", background_color: rgb("#dddddd"), icon: none, icon_color: black) = {
  block(
    breakable: false, 
    fill: background_color, 
    stroke: (paint: icon_color, thickness: 0.5pt, cap: "round"), 
    width: 100%, 
    radius: 2pt,
    block(
      inset: 1pt,
      width: 100%, 
      below: 0pt, 
      block(
        fill: background_color, 
        width: 100%, 
        inset: 8pt)[#text(icon_color, weight: 900)[#icon] #title]) +
      block(
        inset: 1pt, 
        width: 100%, 
        block(fill: white, width: 100%, inset: 8pt, body)))
}



#let article(
  title: none,
  authors: none,
  date: none,
  abstract: none,
  cols: 1,
  margin: (x: 1.25in, y: 1.25in),
  paper: "us-letter",
  lang: "en",
  region: "US",
  font: (),
  fontsize: 11pt,
  sectionnumbering: none,
  toc: false,
  toc_title: none,
  toc_depth: none,
  doc,
) = {
  set page(
    paper: paper,
    margin: margin,
    numbering: "1",
  )
  set par(justify: true)
  set text(lang: lang,
           region: region,
           font: font,
           size: fontsize)
  set heading(numbering: sectionnumbering)

  if title != none {
    align(center)[#block(inset: 2em)[
      #text(weight: "bold", size: 1.5em)[#title]
    ]]
  }

  if authors != none {
    let count = authors.len()
    let ncols = calc.min(count, 3)
    grid(
      columns: (1fr,) * ncols,
      row-gutter: 1.5em,
      ..authors.map(author =>
          align(center)[
            #author.name \
            #author.affiliation \
            #author.email
          ]
      )
    )
  }

  if date != none {
    align(center)[#block(inset: 1em)[
      #date
    ]]
  }

  if abstract != none {
    block(inset: 2em)[
    #text(weight: "semibold")[Abstract] #h(1em) #abstract
    ]
  }

  if toc {
    let title = if toc_title == none {
      auto
    } else {
      toc_title
    }
    block(above: 0em, below: 2em)[
    #outline(
      title: toc_title,
      depth: toc_depth
    );
    ]
  }

  if cols == 1 {
    doc
  } else {
    columns(cols, doc)
  }
}
#show: doc => article(
  title: [GBP Rates Report

],
  authors: (
    ( name: [Andrew Benito],
      affiliation: [],
      email: [] ),
    ),
  date: [2025-09-11],
  font: ("Source Sans Pro",),
  fontsize: 11pt,
  toc: true,
  toc_title: [Table of contents],
  toc_depth: 3,
  cols: 1,
  doc,
)


\#pagebreak\()

= Market developments
<market-developments>
== Latest developments: the past three months
<latest-developments-the-past-three-months>
Summarising some key developments in Sterling markets in the past 3 months: \
- #strong[OIS rates] have seen a 'round-trip', rallying in June by 25bp, selling-off in July and August in moves subsequently reversed. - #strong[Sterling] strengthened against the US Dollar in June \(while OIS rallied), and then reversed that \(as OIS rates sold-off). \
- #strong[Gilt yields] rallied \(by less than OIS) in June, and that decline has been more than reversed since then and especially at the long-end. Sharp rises in August in 25y+ Gilt yields reversed into September. \
- #strong[Equities] rallied strongly by a cumulative 5% over the period, and concentrated in the period from July to mid-August.

This pattern points to greater fiscal risks entering into market pricing, especially in August. While fiscal slippage boosts aggregate demand, some crowding-out from these interest rate responses should also be expected. In the past month, fewer rate cuts have been priced to take place over the next year. Internationally, these interest rate effects appear to have emanated from the US and UK more than from Japan or Germany.

\#pagebreak\()

#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-ois-1.png"))
], caption: figure.caption(
position: bottom, 
[
OIS and Sterling
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-ois>


]
]
#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-gilts-1.png"))
], caption: figure.caption(
position: bottom, 
[
Gilts and Equities
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-gilts>


]
]
== Spreads \(term spreads, swap spreads)
<spreads-term-spreads-swap-spreads>
#strong[Term spreads] Setting some historical context for the recent spread of 10y Gilt yields relative to 2y Gilts yields is @fig-2yv10y. This shows how a term spread has been restored relative to different periods over the past 10 years and can distingsuish severa phases over that period.

#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-2yv10y-1.png"))
], caption: figure.caption(
position: bottom, 
[
Gilt yields
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-2yv10y>


]
]
The 10s25s term spread has widened in recent weeks, and by more than the 2s5s and 5s10s spreads \(@fig-spreads).

#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-spreads-1.png"))
], caption: figure.caption(
position: bottom, 
[
Term spreads in Gilt yields
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-spreads>


]
]
#strong[Swap spreads] shed light on potential effects from bond supply in different markets. Swap spreads have moved more deeply negative in recent weeks. At -xxxbp 5y swap spreads are around their most negative level since xxx. Fiscal developments have likely contributed to this.

#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-swsp-1.png"))
], caption: figure.caption(
position: bottom, 
[
5y swap spreads
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-swsp>


]
]
= Evolving market pricing and OIS curves
<evolving-market-pricing-and-ois-curves>
Reflecting related developments over a longer period, @fig-ois-historical shows how GBP OIS curves have evolved over time, alongside the MPC’s Bank Rate decisions. Persistent forecast errors shown in this chart reflect a gradual process of markets learning about slow-moving, medium-term factors such as the neutral rate and trend productivity growth.

@fig-ois-recent zooms-in on how OIS curves and Bank Rate have evolved over the past year, during which the BoE has lowered its policy rate five times.

#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-ois-historical-1.png"))
], caption: figure.caption(
position: bottom, 
[
Evolution of GBP OIS curves
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-ois-historical>


]
]
#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-ois-recent-1.png"))
], caption: figure.caption(
position: bottom, 
[
More Recent Evolution of GBP OIS curves
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-ois-recent>


]
]
#strong[Neutral rates] We limit our discussion of neutral rates. @fig-neutral shows estimates since the pandemic based on a survey of market participants in Sterling markets. In principle, this survey should embody informed assessments of the impact on neutral rates from a broad set of influences. I simulate individual responses based on the reported summary statistics published by the BoE in its Market Participants Survey \(MaPS).

#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-neutral-1.png"))
], caption: figure.caption(
position: bottom, 
[
Market Participants Survey: Neutral rate estimates
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-neutral>


]
]
= International spillovers in sovereign bond markets
<international-spillovers-in-sovereign-bond-markets>
What of international spillovers in bond markets? Using the Rigobon \(2003) methodology, we can estimate the extent to which international spillovers have contributed to fluctuations in 10y Gilt yields. With bond markets 'spilling over' to one another, this aims to quantify to what extent different markets were 'in the driving seat' at a point in time.

These estimates point to the US and UK being the main 'drivers' of international bond markets over the past decade, with Japan and Germany have been on the receiving end of these spillovers.

#block[
#block[
#figure([
#box(width: 1800.0pt, image("typst-1_files/figure-typst/fig-rigobon-1.png"))
], caption: figure.caption(
position: bottom, 
[
International spillovers
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-rigobon>


]
]
= Fiscal risks
<fiscal-risks>
== The \(international) risk of fiscal fatigue
<the-international-risk-of-fiscal-fatigue>
One Fiscal fatigue occurs where a further fiscal effort is needed to stabilise public debt-to-GDP, but the political will to implement the necessary tax rises or restraint in public spending wanes.

We estimate that the UK still has a required fiscal effort of 3% of GDP to stabilise its Debt-to-GDP ratio. While completing that required fiscal effort is forecast by the OBR on current fiscal plans, the issue including for markets is whether the political will exists, absent risk premiums being priced by markets. Implementation risk crystallised in early July when Parliament failed to pass a Welfare bill with welfare savings.

The risk of fiscal fatigue likely lies behind the periodic risk premiums in Sterling markets, including reflected in term spreads and swap spreads shown previously.

Uncertainty in this estimate of fiscal effort means it is worth showing the debt-stabilising primary balance for different combinations of 'r' \(effective nominal interest rate on government debt) and 'g' \(expected nominal growth rate). Estimates suggest the debt-stabilising primary balance would be aorund 0.5-1.0% of GDP, compared with a primary balance at -2.0% in 2024.

By international standards, this is xxx. \[France stands out.\]

Populism likely adds to this risk of fiscal fatigue, including in the UK and France.

== Macro and fiscal news at UK fiscal events
<macro-and-fiscal-news-at-uk-fiscal-events>
A fiscal downgrade of 30bn would, based on this relationship, point to tightening of A key issue in the Budget will be how front-loaded is that tightening.

#block[
#block[
#figure([
#box(width: 100%,image("typst-1_files/figure-typst/fig-fiscal-fm-1.png"))
], caption: figure.caption(
position: bottom, 
[
Estimates of Actual and Required Fiscal Effort
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-fiscal-fm>


]
]
= Evolving macro forecasts and macro news
<evolving-macro-forecasts-and-macro-news>
Those market reactions may owe partly to the MPC’s own assessment of the UK macro outlook, eg. under the so-called information channel of monetary policy.

#strong[Medium-term] Over the past decade or more, the MPC has tended to revise higher its forecasts for inflation, revise its outlook for unemployment lower, without an obvious bias in its forecasts for GDP growth. This points points to a pattern of the BoE having had to respond to negative supply shocks, updating its assessments in the light of that repeated pattern.

The weakness of productivity growth, negative terms of trade shocks \(e.g.~Brexit, pandemic, Ukraine), and the impact of these on real incomes, have all contributed to this pattern of revisions and evolving macro forecasts.

#strong[Recent updates] Looking over the past six MPC forecasts…

#block[
#block[
#figure([
#box(width: 100%,image("typst-1_files/figure-typst/plot-fe-1.png"))
], caption: figure.caption(
position: bottom, 
[
Distribution of BoE Forecast errors
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


]
]
== Evolving inflation forecasts
<evolving-inflation-forecasts>
Based on the pattern of past BoE forecast errors, a realistic path to overshooting the 2% inflation target, is that the MPC produces too optimistic inflation forecasts and retaining too little policy restriction to complete the disinflation.

#block[
#block[
#figure([
#box(width: 100%,image("typst-1_files/figure-typst/plot-infl-latest-1.png"))
], caption: figure.caption(
position: bottom, 
[
Successive BoE Inflation Forecasts
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


]
]
== Risk premium episodes tilt hawkish for monetary policy
<risk-premium-episodes-tilt-hawkish-for-monetary-policy>
Early-July was \(anoteher) case study of the fiscal fatigue risk premium. The UK Parliament failed to pass a welfare bill of welfare restraint \(modifying the bill before without fiscal savings before passing it). The Chancellor’s tears at the despatch box were widely reported, alongside speculation of a risk Reeves would be replaced and with that the current fiscal rules revised.

The hawkish implied response was reflected in 2y and 5y OIS rates rising by 10-15bp over the following week \(@fig-fatigue). THat was associated with a 2% decline in GBPUSD.

#block[
#block[
#figure([
#box(width: 100%,image("typst-1_files/figure-typst/fig-fatigue-1.png"))
], caption: figure.caption(
position: bottom, 
[
Fiscal fatigue and risk premium in OIS and Sterling
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
numbering: "1", 
)
<fig-fatigue>


]
]
= MPC Voting: increased disagreement
<mpc-voting-increased-disagreement>
We inspect MPC voting patterns to illustrate two points. First, the rising extent of disagreement among MPC members. Second, how current MPC member voting compares with that of past members.

We make the further point that the rising disagreement among MPC members reflects the increasing role for supply-shocks, and more complex macroeconomic environment, in which MPC members are making decisions.

Rising disagreement among MPC members culminated in the August 2025 MPC vote requiring a second vote among Committee members in order to secure a clear majority for the rate cut decision.

Increasingly nuanced macroeconomic assessments are producing more closely contested votes and heightened disagreement among MPC members. This development places greater emphasis on the quality of macroeconomic analysis and forecasts that underpin MPC decisions. While such complex judgements reinforce the importance of maintaining independence from political pressures, the forecast errors documented earlier pose a risk to both the credibility of the MPC’s analytical process and public confidence in its institutional independence. This may leave Sterling markets more sensitive to international spillovers and to political and fiscal news.

#block[
#block[
#figure([
#box(width: 100%,image("typst-1_files/figure-typst/plot-correls-gbp-1.png"))
], caption: figure.caption(
position: bottom, 
[
Rolling correlation: 10y yields and GBPUSD
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


]
]
#block[
#block[
#figure([
#box(width: 100%,image("typst-1_files/figure-typst/plot-correls-ftse-1.png"))
], caption: figure.caption(
position: bottom, 
[
Rolling correlation: 10y yields and Equities
]), 
kind: "quarto-float-fig", 
supplement: "Figure", 
)


]
]
= Conclusions: Macro views
<conclusions-macro-views>
+ Government bonds provide less effective portfolio hedging against equity weakness when negative supply shocks become more frequent. In supply-shock driven downturns, both equities and bonds can decline simultaneously, reducing the traditional negative correlation that makes bonds effective diversifiers.
+ Historical forecast error patterns suggest the MPC faces material upside inflation risks if it maintains current policy settings. The Bank’s tendency to overestimate the pace of disinflation, combined with insufficient recognition of the UK’s structural supply-side constraints, creates conditions where inflation could persistently exceed the 2% target despite current monetary restriction.
+ Fiscal sustainability concerns amplify bond market volatility, particularly during data-sensitive periods when investors reassess inflation trajectories. Disappointing economic data, coming from weak supply-side performance, simultaneously raise questions about fiscal capacity and inflation persistence. In these episodes, we should expect yield curve steepening.
+ The authorities can mitigate yield curve steepening through active debt maturity management, specifically by reducing the average maturity of new issuance. HMT should prioritise other forms of spending restraint and taxes that face lower implementation risk. Front-loading offers similar benefits.
+ If the authorities instead prioritise back-loading fiscal adjustment and/or with higher implementation risk, this would contribute to further steepening. \
+ Fiscal risk premium episodes tilt hawkish for the BoE. Our sense is that this view surprises most analysts.

Fiscal fatigue

#bibliography("references.bib")

