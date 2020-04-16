# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout hakyll-develop

# Build new files
stack exec mysite clean
stack exec mysite build

# Get previous files
git fetch --all
git checkout -b hakyll-experiments --track origin/hakyll-experiments

# Overwrite existing files with new files
cp -a _site/. .

# Commit
echo ">>>>>> Publishing"
git add -A
git commit -m "Publish."

# Push
echo ">>>>>> Pushing"
git push origin hakyll-experiments:hakyll-experiments

# Restoration
echo ">>>>>> Restoration"
git checkout hakyll-develop
git branch -D hakyll-experiments
git stash pop
