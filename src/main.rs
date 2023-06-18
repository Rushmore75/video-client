#![feature(iter_collect_into)]
#![feature(io_error_more)]

use std::{io::{self, Error}, fs, path::{Path, PathBuf}, ffi::{OsString, OsStr}, fmt::Display, time::Duration, process::Command};

use crossterm::{terminal::{enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen, disable_raw_mode}, execute, event::{EnableMouseCapture, DisableMouseCapture, Event, self, KeyCode}};
use tui::{backend::CrosstermBackend, Terminal, widgets::{Block, Borders, List, ListItem, ListState}, style::{Modifier, Style}, layout::{Alignment, Layout, Direction, Constraint}};

fn main() -> Result<(), io::Error> {
    let tree = read_dir(Path::new(".")).unwrap();

    let mut terminal = start_tui()?; 
    let files:          Vec<ListItem<'_>> = tree.files.iter().map(|f| ListItem::from(f)).collect();
    let directories:    Vec<ListItem<'_>> = tree.dirs.iter().map(|f| ListItem::from(f)).collect();

    let files_list = StatefulList::with_items(tree.files_as_contents(), files);
    let directories_list = StatefulList::with_items(tree.directories_as_contents(), directories);

    let mut lists = ToggledStates { list: &mut [files_list, directories_list] };

    loop {
        terminal.draw(|f| {

            let chunks = Layout::default()
                .direction(Direction::Horizontal)
                .constraints([Constraint::Percentage(50), Constraint::Percentage(50)].as_ref())
                .split(f.size());

            let flist = List::new(lists.list[0].items.clone())
                .block(Block::default().title("-<=<| Files |>=>-").title_alignment(Alignment::Center).borders(Borders::ALL))
                .highlight_style(Style::default().add_modifier(Modifier::BOLD))
                .highlight_symbol("👉️");
 
            let dlist = List::new(lists.list[1].items.clone())
                .block(Block::default().title("-<=<| Directories |>=>-").title_alignment(Alignment::Center).borders(Borders::ALL))
                .highlight_style(Style::default().add_modifier(Modifier::BOLD))
                .highlight_symbol("👉️");        

            f.render_stateful_widget(flist, chunks[0], &mut lists.list[0].state);
            f.render_stateful_widget(dlist, chunks[1], &mut lists.list[1].state);
        })?;
       
        if crossterm::event::poll(Duration::from_millis(100))? {
            if let Event::Key(key) = event::read()? {
                match key.code {
                    KeyCode::Char('q') => break,
                    KeyCode::Char('l') => lists.focus_next(),
                    KeyCode::Char('h') => lists.focus_prev(),
                    KeyCode::Char('j') => lists.all_next(), 
                    KeyCode::Char('k') => lists.all_prev(),
                    KeyCode::Enter     => lists.select(), 
                    _ => {}
                }
            }
        }
    };
    stop_tui(terminal)?;
    Ok(())
}

#[derive(Debug)]
enum Contents<'a> {
    File(&'a OsString),
    Directory(&'a Directory),
}

struct ToggledStates<'a, T> {
    list: &'a mut [StatefulList<'a, T>]
}

impl<'a, T> ToggledStates<'a, T> {
    fn focus_next(&mut self) {
        // if nothing is focused just focus the first element
        if self.list.into_iter().find(|f| f.is_focused()).is_none() {
            self.list[0].focus();
            return;
        }

        let mut next = self
            .list
            .iter()
            .enumerate()
            .cycle()
            .skip_while(|t| !t.1.is_focused())
            .take(2);

        let current = next.next().unwrap().0;
        let new = next.next().unwrap().0;
        self.list[current].unfocus();
        self.list[new].focus();
    }

    fn focus_prev(&mut self) {
        // if nothing is focused just focus the first element
        if self.list.into_iter().find(|f| f.is_focused()).is_none() {
            self.list[0].focus();
            return;
        }

        let mut next = self
            .list
            .iter()
            .enumerate()
            .cycle()
            .skip_while(|t| !t.1.is_focused())
            .take(1);
        let index = next.next().unwrap().0;
        let len = self.list.len();

        self.list[index].unfocus();
        if index == 0 {
            self.list[len-1].focus();    
        } else {
            self.list[index-1].focus();
        }
    
    }

    fn all_next(&mut self) {
        self.list.iter_mut().for_each(|f| f.next());
    }

    fn all_prev(&mut self) {
        self.list.iter_mut().for_each(|f| f.previous());
    }

    fn select(&self) {
        self.list.iter().for_each(|f| f.select());
    }
}

struct StatefulList<'a, T> {
    state: ListState,
    items: Vec<T>,
    raw: Vec<Contents<'a>>
}

impl<'a, T> StatefulList<'a, T> {
    fn with_items(raw: Vec<Contents>, items: Vec<T>) -> StatefulList<T> {
        StatefulList {
            state: ListState::default(),
            items,
            raw,
        }
    }

    /// For enter key or similar idea.
    /// Will be ignored if not focused.
    fn select(&self) {
        match self.state.selected() {
            Some(i) => {
                match self.raw[i] {
                    Contents::File(f) => {
                        Command::new("ffplay")
                            .arg(f)
                            .output()
                            .expect("failed to execute ffplay");
                    },
                    Contents::Directory(d) => {
                        // TODO change dir here
                        // Function to server that gets all files and directories, this will be the
                        // same function that will get called at the inception of this program.
                        println!("{:?}", d.path);},
                }
            },
            None => {/*Not selected*/},
        }
    }

    fn focus(&mut self) {
        self.state.select(Some(0))
    }

    fn unfocus(&mut self) {
        self.state.select(None);
    }

    /// go +1 in the list, must be focused first
    fn next(&mut self) {
        if let Some(i) = self.state.selected() {
            if i+1 < self.items.len() {
                self.state.select(Some(i+1));
                return;
            }
            self.state.select(Some(0))
        }
    }

    /// go -1 in the list, must be focused first
    fn previous(&mut self) {
        let x = match self.state.selected() {
            Some(i) => match i {
                0 => Some(self.items.len()-1),
                _ => Some(i-1),
            }
            None => None,
        };
        self.state.select(x);
    }

    fn is_focused(&self) -> bool {
        match self.state.selected() {
            Some(_) => true,
            None => false,
        }
    }

}

fn start_tui() -> Result<Terminal<CrosstermBackend<io::Stdout>>, io::Error> {
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let terminal = Terminal::new(backend)?;
    Ok(terminal)
}

fn stop_tui(mut terminal: Terminal<CrosstermBackend<io::Stdout>>) -> Result<(), io::Error> {

    // restore terminal
    disable_raw_mode()?;
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )?;
    terminal.show_cursor()?;

    Ok(())

}

fn read_dir(path: &Path) -> Result<Directory, Error> {
    if path.is_dir() {
        // It would be nice if I only had to use `read_dir` once, to limit io operations...

        let mut here = Directory::new(Vec::new(), path.to_path_buf(), Vec::new());

        // get the files
        fs::read_dir(path)
            .into_iter()
            .for_each(|r| { r
                .filter_map(|f| f.ok())
                .filter(|f| f.path().is_file())
                .map(|f| File::new(f.file_name()) )
                .for_each(|f| { here.add_file(f) });
            });

        // get the directories
        fs::read_dir(path)
            .into_iter()
            // iterate over the contents of the dir
            .for_each(|dir| { dir
                .filter_map(|entry| entry.ok())
                .map(|p| p.path())
                .filter_map(|p| read_dir(p.as_path()).ok() ) // No need to check if this file is a
                                                             // dir, as it will just error if it's
                                                             // a file. Then it will get filtered out.
                .for_each(|p| {
                    here.add_directory(p)
                });
            });

        Ok(here)
    } else {
        Err(Error::new(io::ErrorKind::IsADirectory, format!("Path \"{:?}\" is to a file, not a directory", path)))
    }
   
}

#[derive(Debug)]
struct File {
    file: OsString,
}

impl File {
    fn new(name: OsString) -> Self {
        File { file: name}
    }
}

impl From<&File> for ListItem<'_> {
    fn from(value: &File) -> Self {
        Self::new(value.file.to_string_lossy().into_owned())
    }
}

impl Display for File {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.file.to_string_lossy())    
    }
}

#[derive(Debug)]
struct Directory {
    files: Vec<File>,
    path: PathBuf,
    dirs: Vec<Directory>,
}

impl Directory {
    fn new(files: Vec<File>, path: PathBuf, dirs: Vec<Directory>) -> Self {
        Directory { files, path, dirs }
    }
    fn add_directory(&mut self, dir: Directory) {
        self.dirs.push(dir);
    }
    
    fn add_file(&mut self, file: File) {
        self.files.push(file);
    }

    fn files_as_contents(&self) -> Vec<Contents> {
        self.files.iter().map(|f| Contents::File(&f.file)).collect()
    }

    fn directories_as_contents(&self) -> Vec<Contents> {
        self.dirs.iter().map(|f| Contents::Directory(f)).collect()
    }

}

impl From<&Directory> for ListItem<'_> {
    fn from(value: &Directory) -> Self {
        // fun times with the compiler...
        let y = value
            .path
            .file_name()
            .unwrap_or(OsStr::new("base dir"))
            .to_string_lossy()
            .into_owned();
        ListItem::new(y)
    }
}

impl Display for Directory {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "> {}", &self.path.file_name().unwrap_or(OsStr::new("")).to_string_lossy())?;

        for i in &self.dirs {
            writeln!(f, "{}/", i)?;
        }
        for i in &self.files {
            writeln!(f, "{}", i.file.to_string_lossy())?;
        }
        write!(f, "{}", "")

    }
}

