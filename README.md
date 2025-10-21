# Trabajo_Pr-ctico_R
Trabajo práctico de R para la materia Laboratorio para el Análisis de Datos Económicos y Financieros.

Materia: Laboratorio para el Análisis de Datos Económicos y Financieros
Universidad Torcuato Di Tella
Autores: Rafael Catalán y Marco Luciano Bunino
Octubre 2025

Este proyecto contiene el desarrollo completo del Trabajo Práctico de Programación en R, dividido en tres ejercicios principales:

  Análisis de datos NBA (2018–2019):
  Se analiza el rendimiento de jugadores según posición, explorando:
    Efectividad de tiros libres.
    Relación entre minutos jugados y puntos anotados.
    Promedio de triples por posición.

  Análisis econométrico con Gapminder:
  Se utiliza información de distintos países para:
    Estimar la evolución del ingreso por persona en Argentina.
    Ajustar modelos lineales y polinómicos.
    Evaluar correlaciones entre países sudamericanos.
    Analizar la relación entre esperanza de vida, género e ingreso.

  Simulación de demanda Cobb–Douglas:
  Se modela el comportamiento de consumo de hogares bajo distintos escenarios:
    Generación de ingresos con distribución Chi-cuadrado.
    Simulación base de demandas y utilidades.
    Efecto de un shock de precios (+20% en el bien 1).
    Inclusión de heterogeneidad en las preferencias de los consumidores.

Requisitos:
El código fue desarrollado en R (versión 4.3 o superior).
Antes de ejecutar los scripts, instalar las siguientes librerías:

install.packages(c("tidyverse", "readr", "readxl"))

Archivos:
  Codigo_TrabajoR.R → Contiene todo el código ejecutable de los tres ejercicios.
  gapminder.csv → Dataset necesario para las partes 1 y 2 del ejercicio de econometría.
  Informe_TP_R → Informe en LaTeX con gráficos y resultados.

Ejecución:
Abrir el archivo Codigo_TrabajoR.R en RStudio.
Asegurarse de tener el archivo gapminder.csv en el mismo directorio de trabajo.
Ejecutar el código por secciones (cada bloque está claramente delimitado por comentarios #----------------------------------------------------------).

Los gráficos y tablas se generan automáticamente en el entorno de R.

Notas adicionales:
  Todos los resultados descriptivos y visuales se presentan en el informe en Overleaf.
  Las funciones auxiliares (simular_ingreso, demanda_cd, prob_bajo_consumo) se definieron dentro del código principal para mantenerlo        autocontenido.
  Los valores numéricos pueden variar levemente por la semilla aleatoria establecida (set.seed(1234)).
