# MWR 29.04.2022

Each raw dataset is stored in the subfolder with the '_raw' suffix.
Each script handling the raw dataset lies at the root of the golem-generated 'data-raw' folder

Each scripts reads the raw dataset, performs some cleaning/modification until the data structure is ready to be fed directly to the app, and then stored in the 'data' folder
	as .Rda which allows for quick reading.

Everytime the raw dataset changes, we should run the .R scripts again. In the future, I could create a overarching .R scripts that runs these dataset-specific scripts all at once.