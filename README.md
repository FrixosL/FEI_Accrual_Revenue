# FEI_Accrual_Revenue

This web applications was created to allow users to "accrue" annual/semi-annual/quarterly transactions into monthly ones.

What does the code actually do:

* Imports a CSV file (eg. Stripe annual transactions)
* Basic formatting of the data into a usable form
* Duplicates each line from the uploaded data set based on the periodicity selected
* Splits the original amount of the transaction based on the periodicity of the transaction (eg. if an annual transaction was $120 and the annual option was selected, then it would be split into $10 per month)
* Distributes the split transactions from above over the next year/half-year/quarter based on the option selected
* Export the data into a CSV file

This code will not be developed further for the time being.