import json

final = {}

with open("kaomoji1.json", "r", encoding="utf-8") as f:
	data = json.loads(f.read())
	for k, v in data.items():
		final[k] = sum([i.lower().split(" ") for i in sum(v.values(), [])], [])

with open("kaomoji2.json", "r", encoding="utf-8") as f:
	data = json.loads(f.read())
	for d in data.values():
		for k, v in d.items():
			for item in v:
				final[item] = [k.lower()]

with open("kaomoji3.json", "r", encoding="utf-8") as f:
	data = json.loads(f.read())
	for d in data:
		for c in d["categories"]:
			for item in c["emoticons"]:
				final[item] = [c["name"].lower()]

with open("emoticon_dict.json", "w", encoding="utf-8") as f:
	f.write(json.dumps(final))
