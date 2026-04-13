# GitHub Pages Setup Instructions

## Enable GitHub Pages

1. Go to your repository: https://github.com/lobsterbush/national-association-visual-arts
2. Click **Settings** (top right)
3. In the left sidebar, click **Pages**
4. Under **Build and deployment**:
   - **Source**: Deploy from a branch
   - **Branch**: `master`
   - **Folder**: `/docs`
5. Click **Save**

GitHub will build and deploy your site. After a few minutes, it will be available at:

**https://lobsterbush.github.io/national-association-visual-arts/**

## Access the Presentation

Once GitHub Pages is enabled:

1. Visit: https://lobsterbush.github.io/national-association-visual-arts/
2. Enter password: `visual-arts`
3. View the presentation

## Structure

- `docs/index.html` - Password-protected landing page
- `docs/presentation.html` - Main reveal.js slide deck
- `output/figures/` - All PNG figures (embedded in presentation)

## Features

- **28 slides** covering survey findings from 890 respondents
- **18 embedded data visualizations**
- **Representative quotes** from open-text responses
- **Colorful, professional design** with pink/purple/blue gradient theme
- **Responsive** for desktop and mobile viewing
- **Password protection** via client-side JavaScript
