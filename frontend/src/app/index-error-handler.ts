import { HttpErrorResponse } from "@angular/common/http";
import { ErrorHandler, Injectable } from "@angular/core";
import { faCoffee, faExclamation, IconDefinition } from "@fortawesome/free-solid-svg-icons";
import { BehaviorSubject, Observable, Subject } from "rxjs";

@Injectable()
export class IndexErrorHandler implements ErrorHandler {

    public error$: Observable<ErrorInfo | null>;
    private _error$: Subject<ErrorInfo | null>;

    constructor() {
        this._error$ = new BehaviorSubject<ErrorInfo | null>(null);
        this.error$ = this._error$;
    }

    handleError(error: any): void {
        if (error instanceof HttpErrorResponse) {
            if (error.status > 500) {
                this._error$.next({
                    icon: faCoffee,
                    message: 'Index service is being updated, please retry after a couple of minutes.',
                    details: error.message
                });
            } else {
                this._error$.next({
                    icon: faExclamation,
                    message: 'Unrecognized error. Please report issue on github',
                    details: error.message
                });
            }
        }
    }

    public clear(): void {
        this._error$.next(null);
    }

}

export interface ErrorInfo {
    message: string;
    details: string;
    icon: IconDefinition;
}
