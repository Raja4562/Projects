import pandas as pd

# Sample cluster data as a Pandas DataFrame
# Replace this with your actual data
data = pd.read_csv("cluster_data.csv")  # Load your preprocessed data

# Group by cluster and calculate the metrics
summary_table = (
    data.groupby("cluster")
    .agg(
        total_items=("count", "sum"),  # Total items in the cluster
        avg_rating=("rating", "mean"),  # Average rating for the cluster
        total_reviews=("reviews", "sum"),  # Total number of reviews for the cluster
    )
    .reset_index()
)

# Print the summary table
print(summary_table)

# Optional: Save the table as a CSV file
summary_table.to_csv("cluster_summary_table.csv", index=False)
