# Admin Console (prototipo Elm + Elm UI)

Prototipo de una consola administrativa hecho con [Elm](https://elm-lang.org) y [mdgriffith/elm-ui](https://package.elm-lang.org/packages/mdgriffith/elm-ui/latest/). Usa datos simulados en memoria (sin backend).

Secciones: Dashboard (KPIs, gráfico, actividad), Usuarios (tabla, búsqueda, alta, activar/desactivar), Pedidos (filtro por estado) y Configuración (tema claro/oscuro, notificaciones).

## Ejecutar

```bash
elm make src/Main.elm --output=elm.js
python3 -m http.server 8000
```

Luego abre <http://localhost:8000>.

## Publicación

Cada push a `main` compila y publica la app en GitHub Pages mediante `.github/workflows/pages.yml` (requiere *Settings → Pages → Source: GitHub Actions*).
