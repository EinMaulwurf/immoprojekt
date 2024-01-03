Here's a step by step guide on how to set up Git on Windows and get started.

1.  Install Git:
    a.  Visit the Git download page at <https://git-scm.com/download/win>.
    b.  The download should start automatically. If not, click the link to download the latest version of Git for Windows.
    c.  Run the downloaded installer and follow the installation steps with the default options.
2.  Configure Git:
    a.  Open Git Bash (search for it in the Start menu).

    b.  Set your name and email using the following commands (replace with your actual name and email):

        ```         
        git config --global user.name "Your Name"
        git config --global user.email "youremail@example.com"
        ```
3.  Generate SSH keys:
    a.  In Git Bash, run the following command (replace with your actual email):

        ```         
        ssh-keygen -t ed25519 -C "youremail@example.com"
        ```

    b.  Press Enter to accept the default file location for the SSH key.

    c.  Enter a secure passphrase when prompted (optional).
4.  Add the SSH key to the ssh-agent:
    a.  Start the ssh-agent in the background:

        ```         
        eval "$(ssh-agent -s)"
        ```

    b.  Add the SSH key to the ssh-agent:

        ```         
        ssh-add ~/.ssh/id_ed25519
        ```
5.  Add the SSH key to your GitHub account:
    a.  Copy the public SSH key to your clipboard:

        ```         
        clip < ~/.ssh/id_ed25519.pub
        ```

    b.  Go to your GitHub account settings: <https://github.com/settings/keys>

    c.  Click "New SSH key" and paste the copied key into the "Key" field.

    d.  Enter a title (e.g., "My Windows PC") and click "Add SSH key".

After setting up Git and configuring SSH keys, you can follow these steps to clone the repository, make changes, and contribute to the project:

1.  Clone the repository:

    a.  Go to the GitHub repository page and click the "Code" button.

    b.  Make sure "SSH" is selected, then copy the SSH URL (it should look like `git@github.com:username/repo.git`).

    c.  Open Git Bash on their local machine and navigate to the folder where they want to store the project.

    d.  Run the following command to clone the repo (replace `<repo_url>` with the copied SSH URL):

        ```         
        git clone <repo_url>
        ```

    e.  The repository will be cloned to a new folder with the same name as the repository.

2.  Create and switch to a new branch:

    a.  Open Git Bash and navigate to the cloned repository folder (`cd path/to/repo`).

    b.  Create a new branch and switch to it using the following command (replace `<branch_name>` with a descriptive name for the branch):

        ```         
        git checkout -b <branch_name>
        ```

3.  Make changes to the project:

    a.  Open the R-Markdown file or other files in the cloned repository using their preferred text editor or RStudio.
    b.  Make changes, add new content, or fix issues as needed.

4.  Commit the changes:

    a.  Open Git Bash and navigate to the cloned repository folder (`cd path/to/repo`).

    b.  Stage the changes:

        ```         
        git add .
        ```

    c.  Commit the changes with a descriptive message:

        ```         
        git commit -m "A brief description of the changes made"
        ```

5.  Push the changes to the remote repository on GitHub, but this time to the new branch:

    ```         
    git push origin <branch_name>
    ```

Now the changes you made will be available in the remote repository on GitHub.
