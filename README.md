# BIOL 242L interactive tools

This repository holds small Shiny apps and other browser-based teaching tools for
BIOL 242L. The structure is intentionally simple so that each activity can be read,
changed, and taught from without first learning a large framework.

## Current activity

`apps/mean-se/app.R` guides students through:

1. entering and inspecting observations from two groups;
2. calculating each group mean;
3. building sample standard deviation from deviations and squared deviations;
4. connecting standard deviation to standard error (`SE = SD / sqrt(n)`); and
5. plotting the two means with ± 2 SE error bars and the raw observations.

The activity calculates with full precision but rounds displayed working for
readability. It uses the usual sample standard deviation, with `n - 1` in the
denominator.

## Run it locally

Install R, then install the app's only runtime dependency once:

```r
install.packages("shiny")
```

From the repository root, run:

```r
shiny::runApp("apps/mean-se")
```

RStudio's **Run App** button also works when `apps/mean-se/app.R` is open.

To preview the exact browser-only version that GitHub Pages will serve:

```r
install.packages(c("shinylive", "httpuv"))
source("scripts/build_site.R")
httpuv::runStaticServer("site")
```

The generated `site/` folder is ignored by Git because GitHub Actions rebuilds it.

## Repository layout

```text
242L_ZTobias/
├── apps/
│   └── mean-se/          # One self-contained Shiny activity
├── scripts/
│   └── build_site.R      # Exports apps for static browser hosting
├── site-source/          # Small landing page copied into the built site
└── .github/workflows/    # Builds and publishes GitHub Pages
```

For another activity, make a new folder under `apps/`. If it is a Shiny app,
add one clearly named `shinylive::export()` call to `scripts/build_site.R` and a
link on `site-source/index.html`. Plain HTML/CSS/JavaScript activities can be kept
in their own folder under `site-source/`.

## Browser deployment

The GitHub Actions workflow exports each Shiny app with
[Shinylive](https://posit-dev.github.io/r-shinylive/). R and Shiny then run in the
student's browser through WebAssembly, so students do not install software and no
Shiny server is needed.

After creating the public GitHub repository `zactobias44/242L_ZTobias`, push the
`main` branch and choose **Settings → Pages → Source → GitHub Actions**. The course
tools will be published under the custom domain already used by the personal site:

<https://zactobias.com/242L_ZTobias/>

GitHub's default project URL, <https://zactobias44.github.io/242L_ZTobias/>,
will redirect there.

Chrome is the preferred browser. The first visit downloads the browser-based R
runtime and can take a few moments; later visits are usually faster because the
browser caches those files.
