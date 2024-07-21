import { CommonModule } from "@angular/common";
import { Component, Input } from "@angular/core";
import { FontAwesomeModule } from "@fortawesome/angular-fontawesome";
import { faSpinner } from '@fortawesome/free-solid-svg-icons';
import { BehaviorSubject, Observable } from "rxjs";

@Component({
    selector: 'app-loader',
    standalone: true,
    imports: [
        CommonModule,
        FontAwesomeModule
    ],
    templateUrl: './loader.component.html',
    styleUrls: ['./loader.component.scss']
})
export class LoaderComponent {

    private _loading$: BehaviorSubject<boolean> = new BehaviorSubject<boolean>(false);
    public loading$: Observable<boolean> = this._loading$.asObservable();

    public spinnerIcon = faSpinner;

    @Input()
    set isLoading(loading: boolean) {
        this._loading$.next(loading);
    }

}
