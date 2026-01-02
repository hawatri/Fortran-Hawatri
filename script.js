// Add any JavaScript functionality here
document.addEventListener('DOMContentLoaded', function() {
    // Common functionality for all pages
    
    // Dark mode toggle functionality
    const darkModeToggle = document.getElementById('dark-mode-toggle');
    if (darkModeToggle) {
        // Initialize dark mode based on system preference or saved preference
        function initializeDarkMode() {
            const isDarkMode = localStorage.getItem('darkMode') === 'true' || 
                              (window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches);
            if (isDarkMode) {
                document.body.classList.add('dark');
                document.body.classList.add('bg-gray-900');
                document.body.classList.remove('bg-white');
                document.body.classList.add('text-white');
                document.body.classList.remove('text-black');
                darkModeToggle.innerHTML = '<i class="fas fa-sun"></i> Light mode';
            }
        }

        // Toggle dark mode
        function toggleDarkMode() {
            const isDarkMode = document.body.classList.contains('dark');
            document.body.classList.toggle('dark');
            
            if (isDarkMode) {
                // Switch to light mode
                document.body.classList.remove('bg-gray-900');
                document.body.classList.add('bg-white');
                document.body.classList.remove('text-white');
                document.body.classList.add('text-black');
                localStorage.setItem('darkMode', 'false');
                darkModeToggle.innerHTML = '<i class="fas fa-moon"></i> Dark mode';
            } else {
                // Switch to dark mode
                document.body.classList.add('bg-gray-900');
                document.body.classList.remove('bg-white');
                document.body.classList.add('text-white');
                document.body.classList.remove('text-black');
                localStorage.setItem('darkMode', 'true');
                darkModeToggle.innerHTML = '<i class="fas fa-sun"></i> Light mode';
            }
        }

        // Set up dark mode toggle
        initializeDarkMode();
        darkModeToggle.addEventListener('click', function(e) {
            e.preventDefault();
            toggleDarkMode();
        });
    }
    
    // Search functionality
    const searchBtn = document.getElementById('search-button');
    const searchInput = document.getElementById('search-input');
    
    if (searchBtn && searchInput) {
        function performSearch() {
            const searchTerm = searchInput.value.toLowerCase();
            if (searchTerm.trim() === '') return;
            
            // For now, we'll just alert the search term
            // In a real implementation, this would search the content
            alert('Searching for: ' + searchTerm);
        }
        
        searchBtn.addEventListener('click', performSearch);
        searchInput.addEventListener('keypress', function(e) {
            if (e.key === 'Enter') {
                performSearch();
            }
        });
    }
}); 