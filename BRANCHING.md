# Git Branching Strategy

This project uses a Git branching strategy to manage development and releases effectively.

## Branches

- **main**: The production branch. Contains stable, released code. Only updated via pull requests from `develop`.
- **develop**: The integration branch for ongoing development. Features are merged here before release.
- **feature/* **: Feature branches for implementing new functionality. Created from `develop` and merged back to `develop` via pull requests.

## Workflow

1. **Start a new feature**:
   ```bash
   git checkout develop
   git pull origin develop
   git checkout -b feature/your-feature-name
   ```

2. **Develop and commit**:
   - Make changes and commit regularly.
   - Push to remote: `git push -u origin feature/your-feature-name`

3. **Create a pull request**:
   - Open a PR on GitHub from `feature/your-feature-name` to `develop`.
   - Ensure CI passes and code review is completed.

4. **Merge to develop**:
   - After approval, merge the PR.
   - Delete the feature branch.

5. **Release to main**:
   - When ready for release, create a PR from `develop` to `main`.
   - Tag the release on `main`.

## Current Branches

- main
- develop
- feature/search-filter (example for implementing search/filter functionality)

## Notes

- Never commit directly to `main` or `develop`.
- Use descriptive branch names for features (e.g., `feature/add-user-auth`).
- Keep feature branches short-lived.