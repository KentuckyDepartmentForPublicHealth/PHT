# Kentucky Public Health Transformation (PHT) Dashboard

A Shiny web application for visualizing and accessing Kentucky Local Health Department (LHD) submissions, accreditation status, and public health priorities.

[![KDPH PHT Dashboard Flyer](/www/KDPH%20PHT%20Dashboard.png)](https://kdph.shinyapps.io/pht-dashboard)

## Overview

This dashboard provides interactive access to Kentucky's local health department data, including Local Needs Assessments (LNA), Community Health Assessments (CHA), and Community Health Improvement Plans (CHIP) as required by Kentucky law.

## Features

### 🗺️ Interactive Map
- Explore Kentucky LHDs with an interactive Leaflet map
- Color-coded visualization by submission status or accreditation
- Downloadable files accessible via location markers
- Customizable labels and display options

![Interactive Map](/www/screenshot/map.png)

### 📊 Analytics Dashboard
- Real-time statistics on LHD submissions and accreditation
- Comparative analysis of local vs. statewide public health priorities
- Data visualization with responsive charts

![Analytics Dashboard](/www/screenshot/dashboard.png)

### 📥 Downloads Center
- Direct access to available LHD documents
- Search and filter by health department
- Organized file listings with one-click downloads

![Downloads Center](/www/screenshot/downloads.png)

### 🔗 Resources Hub
- Links to Kentucky Health Department resources
- State Health Assessment (SHA) and State Health Improvement Plan (SHIP) documents
- Professional development materials

![Resources](/www/screenshot/resources.png)

### 📞 Contact Form
- Integrated feedback system
- Direct submission to Monday.com project management
- Form validation and status tracking

![Contact](/www/screenshot/contact.png)

## Technical Stack

- **Frontend**: Shiny with Bootstrap 5 theming
- **Mapping**: Leaflet for interactive geospatial visualization
- **Data Visualization**: ggplot2 with responsive dark/light mode themes
- **Backend Integration**: RESTful API connection to Monday.com
- **Deployment**: R Shiny Server

## Key Components

- **ui.R**: Multi-panel interface with navbar navigation, sidebar controls, and responsive layouts
- **server.R**: Reactive data processing, map rendering, chart generation, and API integration

## Data Sources

- Kentucky Local Health Department boundaries and metadata
- LHD submission status and accreditation records
- Public health priority selections and comparative state data
- Document repositories for downloadable resources

## Usage

The application automatically loads with the interactive map view. Users can:
1. Navigate between panels using the top navigation bar
2. Use sidebar controls (visible on Home tab) to customize map display
3. Click location markers to access downloadable documents
4. View analytics on the Stats tab
5. Access resources and submit feedback through respective tabs