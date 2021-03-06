# Visµm - Laser Scanning Control GUI

Laser scanning microscope control software GUI written in Pascal using LabView NIDAQ APIs and National Instruments cards.  This is an old project that was written in Delphi 7 (may transfer to Free Pascal / Lazarus IDE without too much headache).  While developed for a very specific hardware setup, the bones of the laser scanning control may be beneficial to those developing or optimizing their own.

![Alt text](Code/VisumSplash.jpg?raw=true "Visum Splash")

# Contents

[1. Code Sections](#1.-Code-Sections)

[2. GUI Features](#-2.-GUI-Features)

[3. Custom Tif Files](#-3.-Custom-Tif-Files)

[4. Laser Scanning Hardware Control](#-4.-Laser-Scanning-Hardware-Control)

#1. Code Sections
Workflow and interactions of different files within the project.  Includes background high priority acquire thread.

![Alt text](Images/code_sections_key.png)![Alt text](Images/code_sections_all.png)
![Alt text](Images/code_sections.png)

[Back To Contents](#-Contents)

# 2. GUI Features

Multi-child window application with static header:

![Alt text](Images/gui_header.png)

Raster image scanning parameters, and example live image:

![Alt text](Images/raster.png) ![Alt text](Images/pollen.png)

Other control options.  Laser power and Scanning FCS parameters:

![Alt text](Images/power_ctrl.png)   ![Alt text](Images/sfcs.png)

Z Stack and XY Stitch control and stage controller:

![Alt text](Images/stack_stitch.png)![Alt text](Images/stage_ctrl.png)

Non-Linear display toggle for bright and dim samples:

![Alt text](Images/non-linear_display.png)

[Back To Contents](#-Contents)

# 3. Custom Tif Files

Modified Tiff 6 standard to add metadata to additional custom tag.  Tag 48000 added to the end of the standard tags.  Single data block contains structure µm width, height, residence time, line and frame settle times, wavelength, and laser power.  See TIFFControl.pas for code details.

![Alt text](CustomTif/TIFFFiles.png)

[Back To Contents](#-Contents)

# 4. Laser Scanning Hardware Control

Standard pixel line and frame synchronized clocks:

![Alt text](ScanControl/CounterPulseGeneration2.png)

Linear ramps with sinusoidal flybacks for faster smooth galvo scanning:

![Alt text](ScanControl/OverclockedCounters3.png)

5x5 image example:

![Alt text](ScanControl/Visio-OverclockedCounters3-5x5Example.png)

[Back To Contents](#-Contents)

