# Academic CV Website (Quarto)

Personal website built with Quarto. This repository includes:

- `index.qmd` (home)
- `research.qmd`
- `teaching.qmd`
- `service.qmd`

## Local preview

1. Install Quarto: <https://quarto.org/docs/get-started/>
2. Run:

```bash
quarto preview
```

## GitHub Pages hosting

This repo includes `.github/workflows/publish.yml` to publish automatically.

1. Push to `main`.
2. In GitHub, go to `Settings -> Pages`.
3. Set source to **GitHub Actions**.
4. The workflow will render and deploy the site.

Quarto is configured to render output into `docs/` via `_quarto.yml`.
