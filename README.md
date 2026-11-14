# ECON 434 — Final Project (Group 14)

Introduction to Big Data Analytics, Fall 2026.
**Topic:** how far the economic outcomes of people in the US in 2022 deviate from pre-pandemic (2019) patterns, relative to a normal three-year change (2016 → 2019). Data: ACS 1-year PUMS 2016, 2019, 2022. Full description — `docs/PROJECT.md`.
**Deadline: 16 November 2026.**

## Repository map

| File / folder | What it is |
|---|---|
| `CLAUDE.md` | instructions for any AI assistant — **read first** (`AGENTS.md` points to it) |
| `docs/PROJECT.md` | full project brief: question, data, methods, all results, limitations — **start here** |
| `docs/REVIEW_GUIDE.md` | how an AI assistant checks the report and slides (rules, reference numbers, common mistakes) |
| `docs/PLAN.md` | work plan, roles, shared calculation rules — **ask your AI "what should I do?"** |
| `docs/GIT_RULES.md` | what and how we push, conditions |
| `docs/WORKLOG.md` | report of work done (newest on top) |
| `docs/IDEAS.md` | ideas, decisions and open questions |
| `docs/AI_USAGE.md` | log of AI use (required by the syllabus) |
| `docs/VARIABLES.md` | which ACS variables we use, could use, and drop — and why |
| `docs/DATA.md` | data structure: tables, keys, pitfalls, sample sizes |
| `code/` | analysis scripts, numbered by step; `code/README.md` describes every script |
| `outputs/` | tables and figures produced by the code; `outputs/README.md` explains every file and lists the results |
| `report/` | project draft |
| `slides/` | presentation |
| `data/` | raw data files — **not tracked in git** |

## Workflow

1. `git pull`
2. Read `docs/WORKLOG.md` (latest entries) and `docs/IDEAS.md` to see where the project is.
3. Do the work, following `docs/GIT_RULES.md`.
4. Update `docs/WORKLOG.md` (and `docs/AI_USAGE.md` if AI helped), then commit and push.

## Reproduce the results

Open `final project.Rproj` in RStudio and run `code/00_run_everything.R` (or the scripts one by one in the order of `code/README.md`). Every script uses relative paths and `set.seed(1)`.

## Data

The raw data are not stored in this repository. Each member downloads them and puts them into `data/`.
Where to get them and how they are structured — see `docs/DATA.md`.
