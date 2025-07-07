# -*- coding: utf-8 -*-

import openai # OpenAI client is used for illustration and can be substituted for the provider in question (e.g., from together import Together)
import pandas as pd
import time
import csv
import tiktoken  # Token counting

# ────────────────────────────────────────────────────────────
# 🔑 Insert API key here
# ────────────────────────────────────────────────────────────
openai.api_key = "OPENAI_API_KEY" # OpenAI client is used for illustration. For other clients use client = CLIENT_NAME(api_key="API_KEY")

# ────────────────────────────────────────────────────────────
# 1  Load the stimulus set
# ────────────────────────────────────────────────────────────
input_file = "llm_meta-task_itemset.csv"  # CSV with “sentence_prompt” and “solution” columns
df = pd.read_csv(input_file)

# ────────────────────────────────────────────────────────────
# 2  Static part of the system message
# ────────────────────────────────────────────────────────────
system_message = {
    "role": "system",
    "content": (
        "You will be given incomplete self-referential statements missing their final word.\n\n"

        "Complete them following these rules:\n\n"

        "- **Ground Rule**: Complete a statement with a single word so the resulting sentence is self-consistent.\n\n"

        "- **Meta Rule**: Track the correct final words across trials. If they form a meaningful sentence "
        "(missing its final word), then on the **next trial**, complete that vertical sentence — "
        "by providing its final word — instead of responding to the presented prompt. This is a **critical meta rule trial**.\n\n"

        "- **Meta-Meta Rule**: If the correct final words on **critical meta rule trials** form a meaningful sentence "
        "(missing its final word), then on the **next critical meta rule trial**, complete that sentence ínstead. \n\n"

        "Example:\n"
        "Prompt: The third word before the last word in this sentence is ___\n"
        "Reasoning: Completing the sentence makes it 12 words long — so the last word is the 12th. "
        "Subtracting 3 from 12 brings us to the 9th word, which is \"this\".\n"
        "Answer: this\n\n"

        "Note: The final word always follows *after the verb*.\n\n"
        "Note: The target word is always within the sentence, even if a referenced word is out of bounds."
        " e.g., 'The second word before the thirteenth word in this sentence is ___.' This sentence doesn't have "
        "a 13th word, but assuming it does, the third word before refers to the 10th word, which is 'sentence'. \n\n"

        "Before answering:\n"
        "- Explain your reasoning.\n"

        "**IMPORTANT: Follow this response format**:\n"
        "Reasoning: [your explanation]\n"
        "Answer: [final word]"
    ),
}

# ────────────────────────────────────────────────────────────
# 3  Tokeniser compatible with the target model
# ────────────────────────────────────────────────────────────
tokenizer = tiktoken.encoding_for_model("gpt-4")

# ────────────────────────────────────────────────────────────
# 4  Experiment loop
# ────────────────────────────────────────────────────────────
results = []
grand_total_tokens = 0
full_trial_history = []

for i, row in df.iterrows():
    trial_prompt = row["sentence_prompt"]
    expected_output = row["solution"]

    # Update sliding window of previous solutions (last 200)
    full_trial_history.append(f"{i + 1}. {expected_output}")
    trial_summary = (
        "\n".join(full_trial_history[max(0, i - 200):i]) if i > 0 else "None yet."
    )

    # Dynamic system message with feedback history
    system_message_dynamic = {
        "role": "system",
        "content": (
            system_message["content"]
            + "\n\nRecord of previous solutions per trial number:\n"
            + trial_summary
        ),
    }

    conversation = [
        system_message_dynamic,
        {"role": "user", "content": trial_prompt},
    ]

    # Input token count
    input_tokens = sum(len(tokenizer.encode(msg["content"])) for msg in conversation)

    try:
        # ──────────────────────────────────────────────────────
        #  Call the language model
        # ──────────────────────────────────────────────────────
        response = openai.chat.completions.create(
            model="MODEL_NAME",        # e.g., "gpt-4o"
            messages=conversation,
            temperature=0,
            max_tokens=4000,
        )

        model_output = response.choices[0].message.content.strip()
        predicted_word = (
            model_output.split("Answer:")[-1].strip().strip(".\"')")
        )
        correct = predicted_word.lower() == expected_output.lower()

        # Output token count
        output_tokens = len(tokenizer.encode(model_output))
        total_tokens = input_tokens + output_tokens
        grand_total_tokens += total_tokens

        print(
            f"Trial {i + 1}: Model said '{predicted_word}' "
            f"(Expected: '{expected_output}') — {'✅' if correct else '❌'}"
        )

        results.append(
            {
                "trial": i + 1,
                "prompt": trial_prompt,
                "expected_output": expected_output,
                "model_output": model_output,
                "predicted_word": predicted_word,
                "correct": correct,
                "input_tokens": input_tokens,
                "output_tokens": output_tokens,
                "total_tokens": total_tokens,
                "context_used": conversation,
            }
        )

    except Exception as e:
        print(f"Trial {i + 1}: ERROR — {e}")
        grand_total_tokens += input_tokens
        results.append(
            {
                "trial": i + 1,
                "prompt": trial_prompt,
                "expected_output": expected_output,
                "model_output": "ERROR",
                "predicted_word": "",
                "correct": False,
                "input_tokens": input_tokens,
                "output_tokens": 0,
                "total_tokens": input_tokens,
                "context_used": conversation,
            }
        )

    # Respect rate limits
    time.sleep(1)

# ────────────────────────────────────────────────────────────
# 5  Save results
# ────────────────────────────────────────────────────────────
output_file = "model_results.csv"
with open(output_file, mode="w", newline="") as file:
    writer = csv.DictWriter(file, fieldnames=results[0].keys())
    writer.writeheader()
    writer.writerows(results)

print(f"\n✅ All trials completed. Results saved to '{output_file}'")
print(f"📊 Total tokens used across all trials: {grand_total_tokens}")
