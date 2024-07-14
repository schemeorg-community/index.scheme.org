import { Component, HostBinding } from '@angular/core';
import { NavigationEnd, Router, RouterModule  } from '@angular/router';
import { filter, map, Observable, startWith, combineLatest } from 'rxjs';
import { IndexService } from './index.service';
import { faHome, faSearch, faFile, faTimes, IconDefinition } from '@fortawesome/free-solid-svg-icons';
import { FontAwesomeModule } from '@fortawesome/angular-fontawesome';
import { FormsModule } from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  standalone: true,
  imports: [
      CommonModule,
      FormsModule,
      RouterModule,
      FontAwesomeModule
  ],
  selector: 'app-root',
  templateUrl: './app.component.html',
  styleUrls: ['./app.component.scss']
})
export class AppComponent {

  navbarItems: Observable<NavbarItem[]>;

  faFile = faFile;
  faTimes = faTimes;

  constructor(filtersetSvc: IndexService, router: Router, public errorHandler: IndexErrorHandler) {
      const routeChange = router.events.pipe(
          filter((event) => event instanceof NavigationEnd),
          startWith(router)
      );
      this.navbarItems = combineLatest([filtersetSvc.filtersets, routeChange]).pipe(
          map(([filtersets, _]) => {
              return [{
                  label: 'Home',
                  icon: faHome,
                  link: '/',
                  isActive: router.isActive('/', {paths: 'exact', queryParams: 'exact', fragment: 'ignored', matrixParams: 'ignored'}),
                  items: []
              }, {
                  label: 'Search',
                  icon: faSearch,
                  isActive: router.isActive(router.parseUrl('/filterset'), {paths: 'subset', queryParams: 'subset', fragment: 'ignored', matrixParams: 'ignored'}),
                  items: filtersets.map(f => {
                      return {
                          label: f.name,
                          link: `filterset/${f.code}/search`,
                          isActive: false,
                          items: []
                      };
                  })
              }];
          }));

      this.selectedTheme = window.localStorage.getItem('theme') || 'default';
  }

  @HostBinding('class.theme-light')
  get lightTheme() {
      const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      return this.selectedTheme == 'light' || (this.selectedTheme == 'default' && !prefersDark);
  }

  @HostBinding('class.theme-dark')
  get darkTheme() {
      const prefersDark = window.matchMedia && window.matchMedia('(prefers-color-scheme: dark)').matches;
      return this.selectedTheme == 'dark' || (this.selectedTheme == 'default' && prefersDark);
  }

  selectedTheme: string;

  public onSelectTheme(theme: string) {
      window.localStorage.setItem('theme', theme);
  }

  public closeErrorWindow() {
      this.errorHandler.clear();
  }

}

interface NavbarItem {
    label: string;
    link?: string;
    icon?: IconDefinition;
    externallink?: boolean;
    items: NavbarItem[];
    isActive: boolean;
}
