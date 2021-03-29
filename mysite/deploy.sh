# Temporarily store uncommited changes
git stash

# Verify correct branch
git checkout hakyll-develop

# Build new files
stack exec mysite clean
stack exec mysite build

# Get previous files
git fetch --all
<<<<<<< HEAD
git checkout -b hakyll-experiments --track origin/hakyll-experiments

# Overwrite existing files with new files
cp -a _site/. .

# Commit
=======
git checkout -b master --track origin/master

# Overwrite existing files with new files
cp -a _site/. ..

# Commit
echo ">>>>>> Publishing"
>>>>>>> hakyll-develop
git add -A
git commit -m "Publish."

# Push
<<<<<<< HEAD
git push origin hakyll-experiments:hakyll-experiments

# Restoration
git checkout hakyll-develop
git branch -D hakyll-experiments
=======
echo ">>>>>> Pushing"
git push origin master:master

# Restoration
echo ">>>>>> Restoration"
git checkout hakyll-develop
git branch -D master
>>>>>>> hakyll-develop
git stash pop
