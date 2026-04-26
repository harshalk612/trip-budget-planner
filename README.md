# 🧳 Trip Budget Planner

An interactive web application built with **R Shiny** that helps travelers plan and manage their trip budgets with real-time weather insights and map-based destination visualization.

---

## 🌟 Features

- 📊 **Expense Tracker** — Log and categorize trip expenses in real time
- 🗺️ **Interactive Map** — Visualize destinations using Leaflet maps
- 🌤️ **Weather Integration** — Fetch real-time weather data for your destination via OpenWeatherMap API
- 📈 **Budget Visualization** — Charts and graphs to monitor spending using ggplot2
- 📋 **Data Tables** — View and manage expense records with interactive DT tables
- 🖥️ **Dashboard UI** — Clean and responsive interface built with shinydashboard

---

## 🛠️ Tech Stack

| Tool | Purpose |
|------|---------|
| R | Core programming language |
| Shiny | Web application framework |
| shinydashboard / shinydashboardPlus | Dashboard UI components |
| Leaflet | Interactive map visualization |
| ggplot2 | Data visualization & charts |
| DT | Interactive data tables |
| owmr | OpenWeatherMap API integration |

---

## 📁 Project Structure

```
trip-budget-planner/
│
├── app.R              # Entry point — loads UI, server, and libraries
├── ui.R               # User interface layout and components
├── server.R           # Backend logic and reactive functions
├── expense tracker.R  # Expense tracking logic
├── .Renviron          # Environment variables (API keys)
└── README.md
```

---

## ⚙️ Installation & Setup

### Prerequisites
- R (>= 4.0)
- RStudio (recommended)

### Steps

1. **Clone the repository**
   ```bash
   git clone https://github.com/harshalk612/trip-budget-planner.git
   cd trip-budget-planner
   ```

2. **Install required packages**
   ```r
   install.packages(c("shiny", "shinydashboard", "shinydashboardPlus",
                      "htmltools", "leaflet", "ggplot2", "owmr", "DT"))
   ```

3. **Set up your API key**

   Create a `.Renviron` file in the root directory and add your OpenWeatherMap API key:
   ```
   OWM_API_KEY=your_api_key_here
   ```
   Get a free key at [openweathermap.org](https://openweathermap.org/api)

4. **Run the app**
   ```r
   shiny::runApp()
   ```

---

## 📄 License

This project is licensed under the [MIT License](LICENSE).

---

## 🙋‍♂️ Author

**Harshal Kakaiya**
🌐 [Portfolio](https://harshalk.netlify.app) · 💼 [LinkedIn](https://www.linkedin.com/in/harshalkakaiya/) · ✍️ [Medium](https://medium.com/@harshal.61/)
