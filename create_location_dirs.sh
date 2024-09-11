#!/bin/bash

# Array of counties and health districts
locations=(
  'Allen County' 'Anderson County' 'Barren River Health District' 'Bell County'
  'Bourbon County' 'Boyd County' 'Boyle County' 'Bracken County'
  'Breathitt County' 'Breckinridge County' 'Buffalo Trace Health District'
  'Bullitt County' 'Calloway County' 'Carter County' 'Christian County'
  'Clark County' 'Cumberland Valley Health District' 'Estill County'
  'Fayette County' 'Fleming County' 'Floyd County' 'Franklin County'
  'Garrard County' 'Gateway Health District' 'Graves County' 'Grayson County'
  'Green River Health District' 'Greenup County' 'Harlan County'
  'Hopkins County' 'Jefferson County' 'Jessamine County' 'Johnson County'
  'Kentucky River Health District' 'Knox County' 'Lake Cumberland Health District'
  'Laurel County' 'Lawrence County' 'Lewis County' 'Lincoln County'
  'Lincoln Trail Health District' 'Madison County' 'Magoffin County'
  'Marshall County' 'Martin County' 'Mercer County' 'Monroe County'
  'Montgomery County' 'Muhlenberg County' 'North Central Health District'
  'Northern Kentucky Health District' 'Oldham County' 'Pennyrile Health District'
  'Pike County' 'Powell County' 'Purchase Health District' 'Three Rivers Health District'
  'Todd County' 'Wedco Health District' 'Whitley County' 'Woodford County'
)

# Loop through the array and create directories for each location
for location in "${locations[@]}"; do
mkdir -p "$location"
touch "$location/.gitkeep"
echo "Directory created: $location"
echo ".gitkeep created inside $location"
done
