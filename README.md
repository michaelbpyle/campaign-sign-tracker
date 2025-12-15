# Sign Delivery Tracker

An interactive map-based application for tracking delivery requests and fulfillment status.

## Features

- **Interactive Map**: Leaflet-based map with colored markers showing delivery status
- **Status Indicators**: Red markers for pending, green for delivered
- **Real-time Statistics**: Dashboard showing total requests, pending, and delivered counts
- **Click-to-Update**: Select any marker to view details and toggle delivery status
- **Delivery List**: Scrollable table of all requests

## Demo Data

This repository uses **fake demonstration data**. All names, phone numbers, and email addresses are fictional. The New Orleans addresses are real for accurate geocoding.

## Two Versions

### Static HTML/JavaScript (`index.html`)

Standalone web page that runs in browser. No server required.

```bash
# Start local server
python -m http.server 8000

# Open http://localhost:8000
```

### R Shiny (`app.R`)

Full Shiny application with data persistence.

```r
install.packages(c("shiny", "leaflet", "dplyr", "readr"))
shiny::runApp()
```

## Project Structure

```
campaign-sign-tracker/
├── index.html                    # Static HTML/JS version
├── app.R                         # R Shiny version
├── data/
│   └── FAKE_DATA_volunteers.csv  # Demo data
└── README.md
```

## Data Format

| Column | Description |
|--------|-------------|
| `id` | Unique identifier |
| `name` | Name |
| `phone` | Phone number |
| `email` | Email address |
| `street_address` | Street address |
| `city` | City |
| `state` | State |
| `zip` | ZIP code |
| `pref_yard_sign` | 1 if requesting delivery |
| `has_sign` | 0 = pending, 1 = delivered |
| `sign_delivered_date` | Delivery date |
| `latitude` | Geocoded latitude |
| `longitude` | Geocoded longitude |

## License

MIT
