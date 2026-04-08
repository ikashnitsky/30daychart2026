import requests, json, time, pandas as pd

repos = [
    "microsoft/vscode",
    "torvalds/linux",
    "facebook/react",
    "vercel/next.js",
    "tensorflow/tensorflow",
    "rust-lang/rust",
    "golang/go",
    "kubernetes/kubernetes",
]

headers = {"Accept": "application/vnd.github+json"}
all_data = []

for repo in repos:
    url = f"https://api.github.com/repos/{repo}/stats/punch_card"
    r = requests.get(url, headers=headers)
    if r.status_code == 200 and r.json():
        for entry in r.json():
            day, hour, count = entry
            all_data.append({"repo": repo, "day": day, "hour": hour, "commits": count})
        print(f"✓ {repo}")
    else:
        print(f"✗ {repo}: {r.status_code}")
    time.sleep(0.5)

df = pd.DataFrame(all_data)
# print(f"\nTotal rows: {len(df)}")
# print(df.head(10))

df.to_csv("dat/github_punch_card_data.csv", index=False)
# print("Data saved to github_punch_card_data.csv")
