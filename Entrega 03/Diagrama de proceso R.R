# ============================================================
# DIAGRAMA DE FLUJO DEL PROYECTO
# Sistema de Evaluacion y Clasificacion de Riesgo Crediticio
# Daniela Lepe Araya
# ============================================================

library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)

# ------------------------------------------------------------
# CREAR DIAGRAMA
# ------------------------------------------------------------

diagrama <- grViz("
digraph riesgo_crediticio {

  graph [
    layout = dot,
    rankdir = TB,
    bgcolor = white,
    nodesep = 0.45,
    ranksep = 0.55
  ]

  node [
    shape = box,
    style = 'rounded,filled',
    fontname = Helvetica,
    fontsize = 12,
    width = 4.5,
    height = 0.8,
    color = '#4F81BD',
    fillcolor = '#EAF2F8',
    penwidth = 1.5
  ]

  edge [
    color = '#4F4F4F',
    penwidth = 1.4,
    arrowsize = 0.8
  ]

  A [
    label = '1. BASE DE DATOS\\ncs-training.csv'
  ]

  B [
    label = '2. EXPLORACION INICIAL\\nRevision de variables y estructura de los datos'
  ]

  C [
    label = '3. LIMPIEZA Y TRANSFORMACION\\nTratamiento de valores faltantes y atipicos\\nEliminacion de variable X'
  ]

  D [
    label = '4. ANALISIS DE VARIABLE OBJETIVO\\nClase 0: 93,4% | Clase 1: 6,6%'
  ]

  E [
    label = '5. DIVISION DE DATOS\\n80% entrenamiento | 20% prueba\\nDivision estratificada'
  ]

  F [
    label = '6. ENTRENAMIENTO DEL MODELO\\nRegresion logistica - glm()\\nFamilia binomial'
  ]

  G [
    label = '7. EVALUACION DEL MODELO\\nMatriz de confusion\\nAccuracy - Sensibilidad - Especificidad - Precision\\nCurva ROC y AUC'
  ]

  H [
    label = '8. SELECCION DEL UMBRAL\\nCriterio de Youden\\nUmbral: 0.0633552'
  ]

  I [
    label = '9. PRODUCTO MINIMO VIABLE (MVP)\\nIngreso de datos de un nuevo cliente\\nEstimacion de probabilidad de riesgo'
  ]

  J [
    label = '10. RESULTADO\\nProbabilidad estimada + Clasificacion\\nApoyo para la toma de decisiones'
  ]

  A -> B
  B -> C
  C -> D
  D -> E
  E -> F
  F -> G
  G -> H
  H -> I
  I -> J
}
")

# ------------------------------------------------------------
# CONVERTIR DIAGRAMA A SVG
# ------------------------------------------------------------

diagrama_svg <- export_svg(diagrama)

# Guardar archivo SVG
writeLines(
  diagrama_svg,
  "diagrama_proceso.svg"
)

# ------------------------------------------------------------
# CONVERTIR SVG A PNG
# ------------------------------------------------------------

rsvg_png(
  charToRaw(diagrama_svg),
  file = "diagrama_proceso.png",
  width = 1600,
  height = 1800
)

# ------------------------------------------------------------
# CONFIRMAR CREACION
# ------------------------------------------------------------

cat(
  "\n============================================\n",
  "DIAGRAMA GENERADO CORRECTAMENTE\n",
  "Archivo: diagrama_proceso.png\n",
  "Ubicacion:", getwd(), "\n",
  "============================================\n"
)