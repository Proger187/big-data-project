# ECON 434 — Final Project (Group 14)

Introduction to Big Data Analytics, Fall 2026.
Topic: migration and remittances (World Bank Migration and Remittances Household Surveys, 2009).
**Deadline: 9 November 2026.**

## Repository map

| File / folder | What it is |
|---|---|
| `CLAUDE.md` | instructions for any AI assistant — **read first** (`AGENTS.md` points to it) |
| `docs/GIT_RULES.md` | what and how we push, conditions |
| `docs/WORKLOG.md` | report of work done (newest on top) |
| `docs/IDEAS.md` | ideas, decisions and open questions |
| `docs/AI_USAGE.md` | log of AI use (required by the syllabus) |
| `docs/DATA.md` | data structure: tables, keys, pitfalls, sample sizes |
| `code/` | analysis scripts (numbered by step) |
| `outputs/` | tables and figures produced by the code |
| `report/` | project draft |
| `slides/` | presentation |
| `data/` | raw survey files — **not tracked in git** |

## Workflow

1. `git pull`
2. Read `docs/WORKLOG.md` (latest entries) and `docs/IDEAS.md` to see where the project is.
3. Do the work, following `docs/GIT_RULES.md`.
4. Update `docs/WORKLOG.md` (and `docs/AI_USAGE.md` if AI helped), then commit and push.

## Data

The raw data are not stored in this repository (World Bank terms of use do not allow redistribution).
Download them from the World Bank Microdata Library and put them into `data/`:

- `NGA_2009_MRHSS_v01_M` — Nigeria Migration and Remittances Household Survey 2009
- `ZAF_2009_MRHSS_v01_M` — South Africa Migration and Remittances Household Survey 2009

Use the Stata (`.dta`) versions. See `docs/DATA.md` for structure.
