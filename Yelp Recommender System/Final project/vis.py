import matplotlib.pyplot as plt
import pandas as pd

# Data for visualization
data = {
    "ItemID": [
        "Crisp",
        "Eataly Chicago",
        "Giordano's",
        "Piece Brewery and Pizzeria",
        "Sunda Chicago",
        "Xoco",
    ],
    "Rating": [4.5, 4.0, 4.0, 4.0, 4.0, 4.0],
    "NumberReview": [3228, 3749, 2790, 3560, 2835, 3646],
    "City": ["Chicago"] * 6,
    "Category": ["Delivery"] * 6,
}

# Create a DataFrame
df = pd.DataFrame(data)

# Sort by NumberReview for visualization
df_sorted = df.sort_values(by="NumberReview", ascending=True)

# Plot horizontal bar chart
plt.figure(figsize=(12, 6))
plt.barh(df_sorted["ItemID"], df_sorted["NumberReview"], color="teal", alpha=0.8)
plt.xlabel("Number of Reviews")
plt.ylabel("Item")
plt.title("Top Rated Items in Chicago (Delivery)")
plt.gca().invert_yaxis()  # Invert y-axis for better readability
plt.show()
