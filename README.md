# gha-live

Live pipeline visualization for GitHub Actions — all workflows and jobs for a commit or PR on a single page, rendered as a color-coded DAG with auto-refresh and clickable links to logs.

## Stack

PureScript · Halogen · SVG · GitHub REST API · esbuild

## Development

Enter the Nix devShell:

```bash
nix develop
```

Available commands:

```bash
just build    # compile PureScript
just bundle   # produce dist/index.js
just dev      # watch mode
just format   # format sources with purs-tidy
just lint     # check formatting
just ci       # lint + build + bundle
just clean    # remove build artifacts
```

Open `dist/index.html` in a browser after bundling.

## License

MIT
