CREATE TABLE IF NOT EXISTS tech_topic (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    date TEXT,
    topic TEXT,
    short_name TEXT,
    points INTEGER
);

CREATE TABLE IF NOT EXISTS note (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    date TEXT,
    content TEXT,
    due_to TEXT,
    status INTEGER
);
