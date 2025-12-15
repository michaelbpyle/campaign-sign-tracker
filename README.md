# Campaign Sign Delivery Tracker

An interactive map-based application for tracking yard sign delivery requests during political campaigns. This project demonstrates real-time geocoded mapping with delivery status management.

![Leaflet Map](https://img.shields.io/badge/Map-Leaflet-green) ![R Shiny](https://img.shields.io/badge/R-Shiny-blue) ![Demo](https://img.shields.io/badge/Data-Demo%20Only-orange)

## Overview

This application was originally developed to manage yard sign deliveries for a political campaign in New Orleans. It provides:

- **Interactive Map**: Leaflet-based map centered on New Orleans showing all sign request locations
- **Visual Status Indicators**: Pulsing red markers for pending deliveries, green for completed
- **Real-time Statistics**: Dashboard showing total requests, pending, and delivered counts
- **Click-to-Update**: Select any marker to view details and toggle delivery status
- **Delivery List**: Sortable table of all requests with status indicators

## Demo Data

**IMPORTANT**: This repository contains **fake demonstration data** only. All volunteer names, phone numbers, and email addresses are completely fictional. Only the New Orleans street addresses are real (for accurate geocoding).

The fake data is located in `data/FAKE_DATA_volunteers.csv` and is clearly marked with header comments indicating its fictional nature.

## Two Versions Included

### 1. Static HTML/JavaScript Version (`index.html`)

A standalone web page that runs entirely in the browser. Perfect for:
- Embedding in WordPress or other CMS
- Hosting on any static file server
- GitHub Pages deployment
- Quick demos without server setup

**Requirements**: Just a web browser and a web server (even `python -m http.server` works)

### 2. R Shiny Version (`app.R`)

A full-featured Shiny application with persistent data storage. Features include:
- Automatic address geocoding via OpenStreetMap
- CSV-based data persistence
- More robust data handling

**Requirements**: R with packages: `shiny`, `leaflet`, `dplyr`, `readr`, `tidygeocoder`

## Quick Start

### Static Version (Recommended for demos)

```bash
# Clone the repository
git clone https://github.com/michaelbpyle/campaign-sign-tracker.git
cd campaign-sign-tracker

# Start a local server (Python 3)
python -m http.server 8000

# Open in browser
# http://localhost:8000
```

### R Shiny Version

```r
# Install dependencies
install.packages(c("shiny", "leaflet", "dplyr", "readr", "tidygeocoder"))

# Run the app
shiny::runApp("path/to/campaign-sign-tracker")
```

## Deployment Options

### GitHub Pages
1. Push to GitHub
2. Go to Settings > Pages
3. Select "Deploy from a branch" > main > / (root)
4. Access at `https://yourusername.github.io/campaign-sign-tracker`

### WordPress Embedding
```html
<iframe
  src="https://yourusername.github.io/campaign-sign-tracker"
  width="100%"
  height="800"
  frameborder="0">
</iframe>
```

### Self-Hosted (Cloudflare Tunnel)
1. Host files on your server
2. Create Cloudflare Tunnel pointing to your server
3. Configure public hostname

## Project Structure

```
campaign-sign-tracker/
├── index.html                    # Static HTML/JS version
├── app.R                         # R Shiny version
├── data/
│   └── FAKE_DATA_volunteers.csv  # Demo data (fictional names)
└── README.md
```

## Data Format

The CSV file expects the following columns:

| Column | Description |
|--------|-------------|
| `id` | Unique identifier |
| `name` | Volunteer name |
| `phone` | Phone number |
| `email` | Email address |
| `street_address` | Street address |
| `city` | City |
| `state` | State abbreviation |
| `zip` | ZIP code |
| `pref_yard_sign` | 1 if requesting a sign |
| `has_sign` | 0 = pending, 1 = delivered |
| `sign_delivered_date` | Date of delivery (if delivered) |
| `latitude` | Geocoded latitude |
| `longitude` | Geocoded longitude |

## Features

- **Pulsing Markers**: Visual distinction between pending (red pulse) and delivered (green pulse)
- **Responsive Design**: Works on desktop and mobile devices
- **No Backend Required**: Static version runs entirely client-side
- **Privacy Focused**: Demo uses only fake personal data

## Original Implementation

This is a portfolio reconstruction of a sign tracker originally built for a judicial campaign. The original implementation included:
- PostgreSQL database backend
- Raspberry Pi deployment
- Google Sheets integration for volunteer signups
- Cloudflare Tunnel for public access
- Automated data ingestion via cron jobs

## License

MIT License - Feel free to adapt for your own campaigns.

## Author

Michael Pyle - [michaelbpyle.com](https://michaelbpyle.com)
