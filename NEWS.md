# SIAmodules 0.1.3

## Major changes

- Following the main `ShinyItemAnalysis` package, the minimum required R version
  was increased to 4.1.0.

## Minor changes

- **CAT Module:**
  - The response pattern generated based on the IRT model and the selected true
    ability is now shown before the CAT simulation, along with some test
    information.

# SIAmodules 0.1.2

## Minor changes

- Wording was updated in all modules.
- **DIF-C module:** Default model was changed to 2PL in order to align with the
  example in the article cited.

## Bugfixes

- **DIF-C module:**
  - Module was fixed to align with recent version of the `{difNLR}` package.
  - Wording was corrected to better fit the concrete DIF-C example discussed.
  - A number of additional bugs were fixed in the module:
    - unsynchronized Summary and Items tabs inputs,
    - active check boxes when they should not be,
    - module crashing when experiencing various convergence errors etc.

# SIAmodules 0.1.1

## Minor changes

- CAT Module: Names in IRT model selection clarified.
- CAT Module: Sample R code and literature added

## Bugfixes

- CAT module: Fixed bug giving false information on correct/incorrect next item answer for Nominal Response Models fitted by ShinyItemAnalysis.
- CAT module: Fixed tooltip in interactive plot.
- DIF-C module: Item selection slider fixed to cover all items.


# SIAmodules 0.1.0

- First release.
